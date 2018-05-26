﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type [<Struct>] Handling =
    | Resolve
    | Cascade

/// Specifies whether an event-based application is running or exiting.
type [<Struct>] Liveness =
    | Running
    | Exiting

/// An entry in the subscription map.
type [<Struct; NoEquality; NoComparison>] SubscriptionEntry =
    { SubscriptionKey : Guid
      Subscriber : Participant
      Callback : obj }

/// An event used by the event system.
type [<Struct; NoEquality; NoComparison>] Event<'a, 's when 's :> Participant> =
    { Data : 'a
      DataType : Type // here so type can be recovered in a dynamic context
      Subscriber : 's
      Publisher : Participant
      Address : 'a Address
      Trace : EventTrace }

/// Abstracts over a subscription sorting procedure.
type 'w SubscriptionSorter =
    SubscriptionEntry array -> 'w -> SubscriptionEntry array

/// Describes an event subscription that can be boxed / unboxed.
type 'w BoxableSubscription =
    obj -> obj -> 'w -> Handling * 'w

/// A map of event subscriptions.
type SubscriptionEntries =
    UMap<obj Address, SubscriptionEntry array>

/// A map of subscription keys to unsubscription data.
type UnsubscriptionEntries =
    UMap<Guid, obj Address * Participant>

module Events =

    /// Represents a wildcard in an event.
    let Wildcard = ntoa<obj> "@"

[<AutoOpen>]
module EventSystemModule =

    /// A publisher-neutral, purely functional event system.
    /// OPTIMIZATION: EventContext mutable for speed.
    type [<ReferenceEquality>] 'w EventSystem =
        private
            { Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              GlobalParticipant : Participant
              mutable EventContext : Participant
              EventStates : UMap<Guid, obj>
              EventTracer : string -> unit
              EventTracing : bool
              EventFilter : EventFilter.Filter
              EventAddresses : obj Address list }

    [<RequireQualifiedAccess>]
    module EventSystem =

        /// The TConfig of Xtension's T/U structures.
        let Config = Functional

        /// Add event state.
        let addEventState<'a, 'w> key (state : 'a) (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = UMap.add key (state :> obj) eventSystem.EventStates }

        /// Remove event state.
        let removeEventState<'w> key (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = UMap.remove key eventSystem.EventStates }

        /// Get subscriptions.
        let getSubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Subscriptions

        /// Get unsubscriptions.
        let getUnsubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Unsubscriptions

        /// Set subscriptions.
        let internal setSubscriptions<'w> subscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Subscriptions = subscriptions }

        /// Set unsubscriptions.
        let internal setUnsubscriptions<'w> unsubscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Unsubscriptions = unsubscriptions }

        /// Get event state.
        let getEventState<'a, 'w> key (eventSystem : 'w EventSystem) =
            let state = UMap.find key eventSystem.EventStates
            state :?> 'a

        /// Get whether events are being traced.
        let getEventTracing<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventTracing

        /// Set whether events are being traced.
        let setEventTracing<'w> tracing (eventSystem : 'w EventSystem) =
            { eventSystem with EventTracing = tracing }

        /// Get the state of the event filter.
        let getEventFilter<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventFilter

        /// Set the state of the event filter.
        let setEventFilter<'w> filter (eventSystem : 'w EventSystem) =
            { eventSystem with EventFilter = filter }

        /// Get the context of the event system.
        let getEventContext (eventSystem : 'w EventSystem) =
            eventSystem.EventContext

        /// Get the global participant of the event system.
        let getGlobalPariticipant (eventSystem : 'w EventSystem) =
            eventSystem.GlobalParticipant

        /// Qualify the event context of the world.
        let qualifyEventContext (address : obj Address) (eventSystem : 'w EventSystem) =
            let context = getEventContext eventSystem
            let contextAddress = context.ParticipantAddress
            let contextAddressLength = Address.length contextAddress
            let addressLength = Address.length address
            if contextAddressLength = addressLength then
                Address.tryTake (contextAddressLength - 1) contextAddress =
                    Address.tryTake (addressLength - 1) address
            elif contextAddressLength < addressLength then
                contextAddress = Address.take contextAddressLength address
            elif contextAddressLength > addressLength then
                address = Address.take addressLength contextAddress
            else false

        /// Set the context of the event system.
        let setEventContext context (eventSystem : 'w EventSystem) =
            eventSystem.EventContext <- context

        /// Log an event.
        let logEvent<'w> (address : obj Address) (trace : EventTrace) (eventSystem : 'w EventSystem) =
            if eventSystem.EventTracing then
                let addressStr = scstring address
                let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
                if EventFilter.filter addressStr traceRev eventSystem.EventFilter then
                    eventSystem.EventTracer (addressStr + "|" + scstring traceRev)

        /// Push an event address to the list for cycle-detection.
        let pushEventAddress<'w> eventAddress (eventSystem : 'w EventSystem) =
            { eventSystem with EventAddresses = eventAddress :: eventSystem.EventAddresses }
            
        /// Pop an event address to the list for cycle-detection.
        let popEventAddress<'w> (eventSystem : 'w EventSystem) =
            { eventSystem with EventAddresses = List.tail eventSystem.EventAddresses }
            
        /// Get the current event address list for cycle-detection.
        let getEventAddresses<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventAddresses

        /// Make an event system.
        let make eventTracer eventTracing eventFilter globalParticipant =
            { Subscriptions = UMap.makeEmpty Config
              Unsubscriptions = UMap.makeEmpty Config
              GlobalParticipant = globalParticipant
              EventContext = globalParticipant
              EventStates = UMap.makeEmpty Config
              EventTracer = eventTracer
              EventTracing = eventTracing
              EventFilter = eventFilter
              EventAddresses = [] }

/// A publisher-neutral, purely functional event system.
type 'w EventSystem = 'w EventSystemModule.EventSystem