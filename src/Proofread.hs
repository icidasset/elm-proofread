{-

ELM PROOFREAD
=============

-}
module Proofread
    ( Document(..)
    , Result(..)
    , Test(..)
    , proofread
    ) where

import Flow
import Proofread.Types
import Protolude

import qualified Data.Text as Text
import qualified Proofread.Parser as Parser


-- 🌳


data Result
    = Ok Document
    | Err Text
    deriving (Show)



-- 📮


proofread :: Text -> Result
proofread contents =
    case Parser.parse contents of
        Parser.Ok document ->
            Ok document

        Parser.Err err ->
            Err err
