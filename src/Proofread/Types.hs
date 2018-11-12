module Proofread.Types where

import Protolude (Show, Text)


-- 🌳


data Document =
    Document Text [ Test ]


data Result ok err
    = Ok ok
    | Err err



-- TESTS


data TestState
    = NotFulfilled
    | Equal
    | Unequal
    | Error Text


data Test = Test
    { input :: Text
    , expectedOutput :: Text
    , state :: TestState
    }



-- SHOW


deriving instance Show Document
deriving instance Show TestState
deriving instance Show Test

deriving instance (Show ok, Show err) => Show (Result ok err)
