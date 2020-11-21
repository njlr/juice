open Expecto
open Juice

let tests =
  [

    test "CE returns simple values" {
      let actual =
        juice {
          return 1
        }
        |> Juice.run ()

      Expect.equal actual.Output 1 "The values should be equal"
    }

    test "CE supports zero" {
      let actual =
        juice {
          ()
        }
        |> Juice.run ()

      Expect.equal actual.Output () "The values should be equal"
    }

    test "CE can set state" {
      let actual =
        juice {
          do! Juice.setState "abc"
          do! Juice.setState "xyz"
        }
        |> Juice.run ""

      Expect.equal actual.State "xyz" "The values should be equal"
    }

    test "CE can get state" {
      let actual =
        juice {
          do! Juice.setState "abc"
          let! state = Juice.getState
          return state + "def"
        }
        |> Juice.run ""

      Expect.equal actual.Output "abcdef" "The values should be equal"
    }

    test "CE can update state" {
      let actual =
        juice {
          do! Juice.updateState ((+) 1)
          do! Juice.updateState ((+) 1)
          do! Juice.updateState ((+) 1)
        }
        |> Juice.run 0

      Expect.equal actual.State 3 "The values should be equal"
    }

    test "CE can emit" {
      let actual =
        juice {
          do! Juice.emit 1
          do! Juice.emit 2
          do! Juice.emit 3
        }
        |> Juice.run ""

      Expect.equal actual.Effects [ 3; 2; 1 ] "The values should be equal"
    }

    test "CE can be nested 1" {
      let actual =
        juice {
          let! x =
            juice {
              return 123
            }

          return string x
        }
        |> Juice.run ""

      Expect.equal actual.Output "123" "The values should be equal"
    }

    test "CE can be nested 2" {
      let actual =
        juice {
          do!
            juice {
              do! Juice.updateState ((+) 1)
            }

          do!
            juice {
              do! Juice.updateState ((+) 1)
            }

          do!
            juice {
              do! Juice.updateState ((+) 1)
            }
        }
        |> Juice.run 0

      Expect.equal actual.State 3 "The values should be equal"
    }

    test "CE supports if-then" {
      let e =
        juice {
          let! state = Juice.getState

          if state % 2 <> 0
          then
            do!
              juice {
                do! Juice.updateState ((*) 2)
              }
        }

      Expect.equal (Juice.run 0 e).State 0 "The values should be equal"
      Expect.equal (Juice.run 1 e).State 2 "The values should be equal"
      Expect.equal (Juice.run 2 e).State 2 "The values should be equal"
      Expect.equal (Juice.run 3 e).State 6 "The values should be equal"
    }

    test "CE supports if-else" {
      let e =
        juice {
          let! state = Juice.getState

          if state >= 10
          then
            return "High score!"
          else
            return "Better luck next time. "
        }

      Expect.equal (Juice.run 0 e).Output "Better luck next time. " "The values should be equal"
      Expect.equal (Juice.run 10 e).Output "High score!" "The values should be equal"
    }

    test "CE supports if-then effects" {
      let e =
        juice {
          let! state = Juice.getState

          if state >= 10
          then
            do! Juice.emit "party.wav"
        }

      Expect.equal (Juice.run 0 e).Effects [] "The values should be equal"
      Expect.equal (Juice.run 10 e).Effects [ "party.wav" ] "The values should be equal"
    }

    test "CE supports for loops" {
      let actual =
        juice {
          for x = 1 to 10 do
            do! Juice.updateState ((+) 1)
        }
        |> Juice.run 0

      Expect.equal actual.State 10 "The values should be equal"
    }

    test "CE supports while loops" {
      let actual =
        juice {
          let mutable keepGoing = true

          while keepGoing do
            let! state = Juice.getState
            do! Juice.setState (state * 2)
            keepGoing <- state < 16
        }
        |> Juice.run 1

      Expect.equal actual.State 32 "The values should be equal"
    }

    test "CE supports juicy while loops" {
      let actual =
        juice {
          let guard =
            juice {
              let! state = Juice.getState
              return state < 10
            }

          while guard do
            do! Juice.updateState ((+) 1)
        }
        |> Juice.run 1

      Expect.equal actual.State 10 "The values should be equal"
    }

    test "CE gathers effects in juicy while loops" {
      let actual =
        juice {
          let guard =
            juice {
              let! state = Juice.getState
              do! Juice.emit "bird.wav"
              return state < 3
            }

          while guard do
            do! Juice.updateState ((+) 1)
        }
        |> Juice.run 1

      let expected =
        [
          "bird.wav"
          "bird.wav"
          "bird.wav"
        ]

      Expect.equal actual.Effects expected "The values should be equal"
    }

    test "CE nesting works correctly" {
      let actual =
        juice {
          let innerJuice =
            juice {
              do! Juice.updateState ((+) 1)
              do! Juice.emit "coinget.wav"
            }

          do!
            Juice.nest
              innerJuice
              (List.head, (fun v xs -> v :: List.tail xs))
              id
        }
        |> Juice.run [ 1 ]

      let expected =
        {
          Output = ()
          State = [ 2 ]
          Effects = [ "coinget.wav" ]
        }

      Expect.equal actual expected "The values should be equal"
    }

  ]
  |> testList "CE"

[<EntryPoint>]
let main args =
  runTestsWithCLIArgs [] args tests
