{-

PARSER
======

-}
module Proofread.Parser
    ( Proofread.Parser.parse
    ) where

import Control.Monad.Combinators
import Data.Text (Text)
import Flow
import Proofread.Parser.Types
import Proofread.Parser.Utilities
import Proofread.Types
import Protolude hiding (and, one, or, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega


-- ⚡️


parse :: Text -> Result Document Text
parse contents =
    let
        success =
            Ok

        failure _ =
            Err "Parse error, invalid Elm module."
    in
    contents
        |> Text.unpack
        |> Mega.parse document ""
        |> either failure success



-- 📮


document :: Parser Document
document = do
    _                   <- maybeSome whitespace
    m                   <- one docModule
    _                   <- maybeSome whitespace
    t                   <- maybeSome test

    return $ Document m t



-- MODULE


docModule :: Parser Text
docModule = do
    _                   <- one (string "module ")
    moduleName          <- some (alphaNumChar `or` char '.' `or` char '_')
    _                   <- one spaceCharacter
    _                   <- manyTill anyChar (string "\n\n")

    return $ Text.pack moduleName



-- TESTS


test :: Parser Test
test =
    try (skipManyTill anyChar testInMultiLineComment)


{-| Parser for a test in a multiline comment.
-}
testInMultiLineComment :: Parser Test
testInMultiLineComment = do
    _                   <- one (string ">>> ")
    startInput          <- someTill anyChar eol
    parserState         <- getParserState
    additionalInput     <- maybeSome (try mlExtra)
    _                   <- maybeSome whitespace
    expectedOutput      <- manyTill anyChar (try mlEnd)

    return $ Test
        { input =
            additionalInput
                |> (<>) [ startInput ]
                |> List.concat
                |> Text.pack

        , expectedOutput = Text.pack expectedOutput
        , lineNumber = getLineNumber parserState
        , state = NotFulfilled
        }


mlExtra :: Parser [Char]
mlExtra = do
    _                   <- some whitespace
    _                   <- one (string "..> ")
    input               <- manyTill anyChar eol

    return ('\n' : input)


mlEnd :: Parser [Char]
mlEnd =
    eol `andThen` maybeSome spaceCharacter `andThen` (eol `or` string "-}")


getLineNumber :: Mega.State a -> Int
getLineNumber state =
    state
        |> Mega.statePos
        |> head
        |> maybe 0 (Mega.sourceLine .> Mega.unPos)
