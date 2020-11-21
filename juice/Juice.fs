namespace Juice

[<Struct>]
type Juice<'output, 'state, 'effect> =
  {
    Output : 'output
    State : 'state
    Effects: 'effect list
  }

type Juicy<'output, 'state, 'effect> = 'state -> Juice<'output, 'state, 'effect>

module Juice =

  // For Aether compatability
  type Lens<'a, 'b> = ('a -> 'b) * ('b -> 'a -> 'a)

  let private (^.) (target : 'a) (lens : Lens<'a,'b>) : 'b =
    let get, _ = lens
    get target

  let private (^=) (value : 'b) (lens : Lens<'a,'b>) : 'a -> 'a =
    let _, set = lens
    set value

  let getState : Juicy<'state, 'state, 'effect> =
    (fun state ->
      {
        State = state
        Output = state
        Effects = []
      })

  let setState state : Juicy<Unit, 'state, 'effect> =
    (fun _ ->
      {
        State = state
        Output = ()
        Effects = []
      })

  let updateState f : Juicy<Unit, 'state, 'effect> =
    (fun state ->
      {
        State = f state
        Output = ()
        Effects = []
      })

  let emit effect : Juicy<Unit, 'state, 'effect> =
    (fun state ->
      {
        State = state
        Output = ()
        Effects = [ effect ]
      })

  let just x : Juicy<_, _, _> =
    (fun state ->
      {
        State = state
        Output = x
        Effects = []
      })

  let ignore (m : Juicy<_, 'state, 'effect>) : Juicy<Unit, 'state, 'effect> =
    (fun state ->
      let x = m state
      {
        Output = ()
        State = x.State
        Effects = x.Effects
      })

  // M<'T> * ('T -> M<'U>) -> M<'U>
  let bind (m : Juicy<'t, 'state, 'effect>) (f : 't -> Juicy<'u, 'state, 'effect>) : Juicy<'u, 'state, 'effect> =
    (fun state ->
      let x1 = m state
      let x2 = (f x1.Output) x1.State
      {
        Output = x2.Output
        State = x2.State
        Effects = x2.Effects @ x1.Effects
      })

  // (unit -> M<'T>) -> M<'T>
  let delay f : Juicy<_, _, _> = f ()

  // M<unit> * M<'T> -> M<'T>
  let combineUnit (m1 : Juicy<Unit, _, _>) (m2 : Juicy<_, _, _>) : Juicy<_, _, _> =
    (fun state ->
      let x1 = m1 state
      let x2 = m2 x1.State
      {
        Output = x2.Output
        State = x2.State
        Effects = x2.Effects @ x1.Effects
      })

  // M<'T> * M<'T> -> M<'T>
  // let combine (m1 : Juicy<_, _, _>) (m2 : Juicy<_, _, _>) : Juicy<_, _, _> =
  //   (fun state ->
  //     let x1 = m1 state
  //     { x1 with Effects = x2.Effects @ x1.Effects })

  let run state m =
    m state

  let nest (inner : Juicy<'output, 'innerState, 'innerEffect>) (lens : Lens<'outerState, 'innerState>) (effectHandler) : Juicy<'output, 'outerState, 'outerEffect> =
    (fun outerState ->
      let innerState = outerState ^. lens
      let x = inner innerState
      {
        Output = x.Output
        State = outerState |> x.State ^= lens
        Effects = x.Effects |> List.map effectHandler
      }
    )

  let forEach (xs : seq<_>) (f : _ -> Juicy<_, _, _>) : Juicy<_, _, _> =
    xs
    |> Seq.fold
      (fun acc x -> combineUnit (f x) acc)
      (just ())


[<AutoOpen>]
module ComputationExpression =

  open Juice

  // { if expr then cexpr0 |}  if expr then { cexpr0 } else builder.Zero()

  // { for pattern in expr do cexpr }  builder.For(enumeration, (fun pattern -> { cexpr }))

  // { while expr do cexpr }  builder.While(fun () -> expr, builder.Delay({ cexpr }))

  type JuiceBuilder () =
    member this.Bind (m, f) =
      bind m f

    member this.Delay (f) =
      delay f

    member this.Combine (m1, m2) =
      combineUnit m1 m2

    // member this.Combine (m2, m1) =
    //   combineUnit m1 m2

    // member this.Combine (m1, m2) =
    //   combine m1 m2

    member this.For (xs, f) =
      forEach xs f

    member this.While (guard, m) =
      if guard ()
      then
        this.Bind (m, fun () -> this.While (guard, m))
      else
        this.Zero ()

    member this.While (guard : Unit -> Juicy<bool, 'state, _>, m : Juicy<Unit, 'state, _>) : Juicy<Unit, 'state, _> =
      (fun (state : 'state) ->
        let x1 = guard () state

        if x1.Output
        then
          let x2 = this.Bind (m, fun () -> this.While (guard, m)) x1.State

          {
            Output = x2.Output
            State = x2.State
            Effects = x1.Effects @ x2.Effects
          }
        else
          {
            Output = ()
            State = x1.State
            Effects = x1.Effects
          })

    member this.Return x =
      just x

    member this.Zero () =
      just ()

  let juice = JuiceBuilder ()
