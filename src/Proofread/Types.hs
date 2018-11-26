module Proofread.Types where

import Protolude (Eq, Int, Show, Text)


-- 🌳


data Document =
    Document Text [ Test ]


data Result ok err
    = Ok ok
    | Err err



-- TESTS


data TestState
    = NotFulfilled
    | PrepareStatement
    | Equal
    | Unequal Text
    | Error Text


data Test = Test
    { input :: Text
    , expectedOutput :: Text
    , lineNumber :: Int
    , state :: TestState
    }



-- INSTANCES


deriving instance Show Document
deriving instance Show TestState
deriving instance Show Test

deriving instance Eq Document
deriving instance Eq TestState
deriving instance Eq Test

deriving instance (Show ok, Show err) => Show (Result ok err)
