module Proofread.Types where

import Data.Text (Text)
import Protolude (Show)


-- 🌳


data Document =
    Document Text [ Test ]
    deriving (Show)


data Test = Test
    { input :: Text, output :: Text }
    deriving (Show)
