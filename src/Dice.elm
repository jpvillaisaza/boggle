module Dice exposing (..)

import List.Extra
import Random
import Random.Extra
import Random.List


type alias Dice a =
    { sides : List a
    }


roll : a -> Dice a -> Random.Generator a
roll default dice =
  let
      n = List.length dice.sides

      helper i =
          case List.Extra.getAt i dice.sides of
              Nothing ->
                  default

              Just element ->
                  element
  in
      Random.map helper (Random.int 0 (n - 1))


gene : a -> List (Dice a) -> Random.Generator (List a)
gene default dice =
    let
        helper list =
            Random.Extra.combine (List.map (roll default) list)
    in
        Random.List.shuffle dice
            |> Random.andThen helper
