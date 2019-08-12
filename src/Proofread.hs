{-

ELM PROOFREAD
=============

-}
module Proofread
    ( module Proofread.Types
    , proofread
    ) where

import Flow
import Proofread.Types
import Protolude

import qualified Proofread.Parser as Parser
import qualified Proofread.Runner as Runner


-- 📮


proofread :: Text -> IO (Result Document Text)
proofread contents =
    case Parser.parse contents of
        Err err             -> return (Err err)
        Ok (Document m [])  -> return (Ok <| Document m [])
        Ok document         -> Runner.run document
