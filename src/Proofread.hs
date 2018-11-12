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
import qualified Proofread.Runner as Runner


-- 📮


proofread :: Text -> IO (Result Document Text)
proofread contents =
    case Parser.parse contents of
        Ok document -> Runner.run document >>=
                        (\doc -> do
                            print (show doc :: [Char])
                            return doc
                        )
        Err err     -> return (Err err)
