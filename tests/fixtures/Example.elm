module Example exposing (AdventureTime(..), add)

import Dict


{-| Adventure time.

    >>> import Dict

    >>> testPrep =
    ..>   Dict.empty

    >>> Finn
    Finn

-}
type AdventureTime
    = Finn
    | Jake


{-|

    >>> add 1 2
    3

    >>> import Tuple
    >>> nine =
    ..>   Tuple.first ( 9, True )

    >>> 1.0
    ..>     |> round
    ..>     |> add nine
    add
        (round 5.0)
        5

    Hello ✌️

-}
add : number -> number -> number
add x y =
    Debug.log "ignoreThis" (x + y)
