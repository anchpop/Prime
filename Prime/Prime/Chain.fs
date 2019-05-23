// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2019.

namespace Prime
open System.Diagnostics
open Prime

/// The Chain monad - Essentially the State monad, will provide a restricted interface to keep everything safe.
type [<NoComparison; NoEquality>] Chain<'w, 'e, 'a> =
    Chain of (('w * 'e) -> ('w * 'e * 'a))


/// Implements the chain monad.
type ChainBuilder () =
    let unChain a = match a with Chain x -> x

    /// Functor map for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Map f c =
        let sp = match c with Chain sp -> sp 
        in Chain (fun (w, e) -> (let output = sp(w, e)
                                 in match output with  (w_output, e_output, a_output) -> (w_output, e_output, f a_output)))
        

    /// Applicative apply for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Apply (v1 : Chain<'w, 'e, ('a -> 'b)>) (v2 : Chain<'w, 'e, 'a>) : Chain<'w, 'e, 'b> =
        let threadWorldThroughAndApply(w, e) = match unChain(v1)(w, e) with (w1_output, e1_output, a1_output) -> (match unChain(v2)(w1_output, e1_output) with (w2_output, e2_output, a2_output) -> (w2_output, e2_output, a1_output a2_output))
        in Chain threadWorldThroughAndApply

    /// Applicative return for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return a =
        Chain (fun (w, e) -> (w, e, a))

    /// Monadic bind for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Bind (valToGive : Chain<'w, 'e, 'a>, funcToApply : 'a -> Chain<'w, 'e, 'b>) : Chain<'w, 'e, 'b> =
            let threadWorldThroughAndApply (w : 'w, e : 'e) : ('w * 'e * 'b) = 
                let (w1_output, e1_output, a1_output) = unChain(valToGive)(w, e)
                let (Chain func) = funcToApply a1_output
                func(w1_output, e1_output)
            Chain threadWorldThroughAndApply

[<AutoOpen>]
module ChainBuilder =

    /// Builds the chain monad.
    let chain = ChainBuilder ()

[<RequireQualifiedAccess>]
module Chain =

    /// Functor map for the chain monad.
    let inline map f a = chain.Map f a

    /// Functor map for the chain monad.
    let inline apply m a = chain.Apply m a

    /// Monadic return for the chain monad.
    let inline returnM a = chain.Return a

    /// Monadic bind for the chain monad.
    let inline bind m a = chain.Bind(m, a)



    /// Get the world.
    let get : Chain<'w, 'e, 'w> = Chain (fun (w, e) -> (w, e, w))

    /// Get the world as transformed via 'by'.
    let getBy (by : 'w -> 'a) : Chain<'w, 'e, 'a> = Chain (fun (w, e) -> (w, e, by w))

    /// Set the world.
    let [<DebuggerHidden; DebuggerStepThrough>] set w : Chain<'w, 'e, unit> = Chain (fun (_, e) -> (w, e, ()))

    /// Update the world with an additional transformed world parameter.
    let [<DebuggerHidden; DebuggerStepThrough>] updateBy by expr : Chain<'w, 'e, unit> = Chain (fun (w, e) -> (expr (by w) w, e, ()))

    /// Update the world.
    let [<DebuggerHidden; DebuggerStepThrough>] update expr : Chain<'w, 'e, unit> = Chain (fun (w, e) -> (expr w, e, ()))

    /// Get the next event.
    let next : Chain<'w, 'e, 'e> = failwith "Not implemented yet"

    /// Pass over the next event.
    let pass : Chain<'w, 'e, unit> = failwith "Not implemented yet"

    /// React to the next event, using the event's data in the reaction.
    // TODO: See if we can make this acceptable to F#'s type system -
    //let [<DebuggerHidden; DebuggerStepThrough>] reactD<'a, 's, 'e, 'w when 's :> Participant and 'e :> Event<'a, 's> and 'w :> EventSystem<'w>> expr : Chain<'e, unit, 'w> =
    //    chain {
    //        let! e = next
    //        let! world = get
    //        let world = expr (e.Data) world
    //        do! set world }

    /// React to the next event, using the event's value in the reaction.
    let [<DebuggerHidden; DebuggerStepThrough>] reactE expr : Chain<'e, unit, 'w> = failwith "Not implemented yet"

    /// React to the next event, discarding the event's value.
    let [<DebuggerHidden; DebuggerStepThrough>] react expr : Chain<'e, unit, 'w> = failwith "Not implemented yet"

    /// Loop in a chain context while 'pred' evaluate to true.
    let rec [<DebuggerHidden; DebuggerStepThrough>] loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> 'w -> bool) (m : 'i -> Chain<'e, unit, 'w>) = failwith "Not implemented yet"

    /// Loop in a chain context while 'pred' evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] during (pred : 'w -> bool) (m : Chain<'e, unit, 'w>) = failwith "Not implemented yet"

    /// Step once into a chain.
    let [<DebuggerHidden; DebuggerStepThrough>] step (m : Chain<'e, 'a, 'w>) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> = failwith "Not implemented yet"

    /// Advance a chain value by one step, providing 'e'.
    let [<DebuggerHidden; DebuggerStepThrough>] advance (m : 'e -> Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> = failwith "Not implemented yet"

    /// Run a chain to its end, providing 'e' for all its steps.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run3 (m : Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : ('w * 'a) = failwith "Not implemented yet"

    /// Run a chain to its end, providing unit for all its steps.
    let [<DebuggerHidden; DebuggerStepThrough>] run2 (m : Chain<unit, 'a, 'w>) (world : 'w) : ('w * 'a) = failwith "Not implemented yet"

    /// Run a chain to its end, providing unit for all its steps.
    let [<DebuggerHidden; DebuggerStepThrough>] run (m : Chain<unit, 'a, 'w>) (world : 'w) : 'w = failwith "Not implemented yet"

    let private run4 handling (chain : Chain<Event<'a, Participant>, unit, 'w>) (stream : Stream<'a, 'w>) (world : 'w) =
        let globalParticipant = EventSystem.getGlobalParticipantGeneralized world
        let stateKey = makeGuid ()
        let subscriptionKey = makeGuid ()
        let world = EventSystem.addEventState stateKey (fun (_ : Event<'a, Participant>) -> chain) world
        let (eventAddress, unsubscribe, world) = stream.Subscribe world
        let unsubscribe = fun world ->
            let world = EventSystem.removeEventState stateKey world
            let world = unsubscribe world
            EventSystem.unsubscribe subscriptionKey world
        let advance = fun evt world ->
            let chain = EventSystem.getEventState stateKey world : Event<'a, Participant> -> Chain<Event<'a, Participant>, unit, 'w>
            let (world, advanceResult) = advance chain evt world
            match advanceResult with
            | Right () -> unsubscribe world
            | Left chainNext -> EventSystem.addEventState stateKey chainNext world
        let subscription = fun evt world ->
            let world = advance evt world
            (handling, world)
        let world = advance Unchecked.defaultof<Event<'a, Participant>> world
        let world = EventSystem.subscribePlus<'a, Participant, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
        (unsubscribe, world)

    /// Run a chain over Prime's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runAssumingCascade chain (stream : Stream<'a, 'w>) world = failwith "Not implemented yet"

    /// Run a chain over Prime's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runAssumingResolve chain (stream : Stream<'a, 'w>) world = failwith "Not implemented yet"