﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime.Tests
open System
open Xunit
open Prime
open Prime.Stream
open Prime.Chain
module EventTests =

    type [<StructuralEquality; NoComparison>] TestParticipant =
        { TestAddress : TestParticipant Address }
        interface Participant with
            member this.ParticipantAddress = atoa<TestParticipant, Participant> this.TestAddress
            end

    type [<ReferenceEquality>] TestWorld =
        { TestState : int
          TestEventSystem : TestWorld EventSystem }
        interface EventWorld<TestParticipant, TestWorld> with
            member this.GetLiveness () = Running
            member this.GetEventSystem () = this.TestEventSystem
            member this.UpdateEventSystem updater = { this with TestEventSystem = updater this.TestEventSystem }
            member this.ParticipantExists participant = participant.GetType () = typeof<TestParticipant>
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                match participant with
                | :? TestParticipant -> EventWorld.publishEvent<'a, 'p, TestParticipant, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> failwithumf ()
        static member incTestState this =
            { this with TestState = inc this.TestState }
        static member make eventTracer eventTracing eventFilter globalParticipant =
            { TestState = 0; TestEventSystem = EventSystem.make eventTracer eventTracing eventFilter globalParticipant }

    let TestEvent = ntoa<int> "Inc"
    let TestEvent2 = ntoa<bool> "Flag"
    let TestParticipant = { TestAddress = Address.empty<TestParticipant> }
    let incTestState _ world = TestWorld.incTestState world
    let incTestStateNoEvent world = TestWorld.incTestState world
    let incTestStateTwice _ world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateTwiceNoEvent world = TestWorld.incTestState (TestWorld.incTestState world)
    let incTestStateAndCascade (_ : Event<int, TestParticipant>) world = (Cascade, TestWorld.incTestState world)
    let incTestStateAndResolve (_ : Event<int, TestParticipant>) world = (Resolve, TestWorld.incTestState world)

    let [<Fact>] subscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = EventWorld.subscribe incTestState TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = EventWorld.subscribe incTestState TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = EventWorld.subscribe incTestState TestEvent TestParticipant world
        let world = EventWorld.subscribe incTestState TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = EventWorld.subscribePlus (makeGuid ()) incTestStateAndResolve TestEvent TestParticipant world |> snd
        let world = EventWorld.subscribePlus (makeGuid ()) incTestStateAndCascade TestEvent TestParticipant world |> snd
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] unsubscribeWorks () =
        let key = makeGuid ()
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = EventWorld.subscribePlus key incTestStateAndResolve TestEvent TestParticipant world |> snd
        let world = EventWorld.unsubscribe key world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (0, world.TestState)

    let [<Fact>] streamWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world = stream TestEvent |> subscribe incTestState TestParticipant <| world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamSubscribeTwiceUnsubscribeOnceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let stream = stream TestEvent
        let world = subscribe incTestState TestParticipant stream world
        let (unsubscribe, world) = subscribePlus incTestStateAndCascade TestParticipant stream world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamUnsubscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let (unsubscribe, world) = stream TestEvent |> subscribePlus incTestStateAndCascade TestParticipant <| world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))
        Assert.Equal (0, world.TestState)

    let [<Fact>] filterWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world =
            stream TestEvent |>
            filterWorld (fun _ world -> world.TestState = 0) |>
            subscribe incTestState TestParticipant <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] mapWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world =
            stream TestEvent |>
            map (fun a -> a * 2) |>
            subscribe (fun evt world -> { world with TestState = evt.Data }) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] productWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world =
            product (stream TestEvent) (stream TestEvent) |>
            subscribe (fun evt world -> { world with TestState = fst evt.Data + snd evt.Data }) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] sumWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world =
            sum (stream TestEvent) (stream TestEvent2) |>
            subscribe (fun evt world -> { world with TestState = match evt.Data with Left i -> i | Right _ -> 10 }) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)
        let world = EventWorld.publish true TestEvent2 EventTrace.empty TestParticipant world
        Assert.Equal (10, world.TestState)

    let [<Fact>] scanWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let world =
            stream TestEvent |>
            fold (+) 0 |>
            subscribe (fun evt world -> { world with TestState = evt.Data }) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (3, world.TestState)

    let [<Fact>] scan2DoesntLeaveGarbage () =
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let (unsubscribe, world) =
            stream TestEvent |>
            reduce (+) |>
            subscribePlus incTestStateAndCascade TestParticipant <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = unsubscribe world
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))

    let [<Fact>] chainWorks () =
        
        // build everything
        let world = TestWorld.make ignore false EventFilter.Empty TestParticipant
        let chain =
            chain {
                let! e = next
                do! update (incTestState e)
                do! react incTestStateNoEvent
                do! reactE incTestState
                do! pass
                do! loop 0 inc (fun i _ -> i < 2) (fun _ -> update incTestStateTwiceNoEvent) }
        let stream = stream TestEvent
        let world = Chain.runAssumingCascade chain stream world |> snd
        Assert.Equal (0, world.TestState)

        // assert the first publish executes the first chained operation
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

        // assert the second publish executes the second chained operation
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 3 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (3, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 4 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (7, world.TestState)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True (UMap.isEmpty (EventWorld.getSubscriptions world))