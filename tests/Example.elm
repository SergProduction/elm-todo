module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TaskStruct exposing (Task(..), mapTask, filterTask)


-- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"

suite : Test
suite = describe "task module"
    [ describe "map"
        [ test "change pos through map" <|
          \_ ->
            let
                task = Task { name = "root"
                            , pos = 0
                            , lvl = 0
                            , children = [
                              Task { name = "1 lvl"
                              , pos = 0
                              , lvl = 1
                              , children = []
                              }
                            ]
                            }
                shouldBeTask = Task { name = "root"
                            , pos = 3
                            , lvl = 0
                            , children = [
                              Task { name = "1 lvl"
                              , pos = 3
                              , lvl = 1
                              , children = []
                              }
                            ]
                            }
            in
              Expect.equal
              shouldBeTask
              <| mapTask (\(Task t) -> Task { t | pos = t.pos + 3 }) task
        , test "filter task" <|
          \_->
            let
                task = Task { name = "root"
                            , pos = 0
                            , lvl = 0
                            , children = [
                              Task { name = "1 lvl"
                              , pos = 0
                              , lvl = 1
                              , children = [
                                Task { name = "2 lvl"
                                , pos = 0
                                , lvl = 2
                                , children = [
                                  Task { name = "3 lvl"
                                  , pos = 0
                                  , lvl = 3
                                  , children = []
                                  }
                                ]
                                }
                              ]
                              }
                            ]
                            }
                shouldBeTask = Task { name = "root"
                            , pos = 0
                            , lvl = 0
                            , children = [
                              Task { name = "1 lvl"
                              , pos = 0
                              , lvl = 1
                              , children = [
                                Task { name = "2 lvl"
                                , pos = 0
                                , lvl = 2
                                , children = []
                                }
                              ]
                              }
                            ]
                            }
            in
              Expect.equal
              shouldBeTask
              ( filterTask (\(Task t) -> not (t.pos == 0 && t.lvl == 3) ) task )
        ]
    ]
