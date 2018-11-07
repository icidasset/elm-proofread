module Proofread.Parser.Types where

import Data.Void (Void)
import Protolude (Char)
import Text.Megaparsec (Parsec)


-- 🌳


type Parser = Parsec Void [Char]
