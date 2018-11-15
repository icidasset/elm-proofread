module Example exposing (AdventureTime(..), add)

import Dict


{-| Adventure time.

    >>> Finn
    Finn

-}
type AdventureTime
    = Finn
    | Jake


{-|

    >>> add 1 2
    3

    >>> 1.0
    ..>     |> round
    ..>     |> add 9
    add
        (round 5.0)
        5

    Hello ✌️

-}
add : number -> number -> number
add x y =
    x + y
