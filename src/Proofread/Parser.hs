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
import Proofread.Parser.Common
import Proofread.Parser.Types
import Proofread.Types
import Protolude hiding (and, moduleName, one, or, some, state, try)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega


-- ⚡️


parse :: Text -> Result Document Text
parse contents =
    let
        ok =
            Ok

        err _ =
            Err "Parse error, invalid Elm module."
    in
    contents
        |> Text.unpack
        |> Mega.parse document []
        |> either err ok



-- 📮


document :: Parser Document
document = do
    _                   <- maybeSome whitespace
    _                   <- optional (string "port")
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
    _                   <- some whitespace
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
    _                   <- maybeSome spaceCharacter
    expectedOutput      <- manyTill anyChar (try <| lookAhead <| prepEnd `or` mlEnd)

    return $ Test
        { input =
            additionalInput
                |> (<>) [ startInput ]
                |> List.concat
                |> Text.pack

        , state =
            if (Text.pack .> Text.strip) expectedOutput == "" then
                PrepareStatement
            else
                NotFulfilled

        , expectedOutput    = Text.pack expectedOutput
        , lineNumber        = getLineNumber parserState
        }


mlExtra :: Parser [Char]
mlExtra = do
    _                   <- some whitespace
    _                   <- one (string "..> ")
    input               <- manyTill anyChar eol

    return ('\n' : input)


mlEnd :: Parser [Char]
mlEnd =
    eol `andThen` maybeSome spaceCharacter `andThen` (eol `or` prepEnd)


prepEnd :: Parser [Char]
prepEnd =
    string "-}" `or` string ">>> "


getLineNumber :: Mega.State a -> Int
getLineNumber state =
    state
        |> Mega.statePos
        |> head
        |> maybe 0 (Mega.sourceLine .> Mega.unPos)
