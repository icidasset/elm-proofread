module Example exposing (AdventureTime(..), add)

{-| --- | (• ◡•)| (❍ᴥ❍ʋ)
-}

import Task


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

-}
add : number -> number -> number
add x y =
    x + y
