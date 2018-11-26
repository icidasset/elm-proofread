{-

RUNNER
======

-}
module Proofread.Runner where

import Control.Concurrent
import Control.Monad.Combinators
import Flow
import Proofread.Parser.Types (Parser)
import Proofread.Parser.Utilities
import Proofread.Types
import Protolude hiding (or, state)
import System.IO
import System.Process.Typed
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO


-- ⚡️


run :: Document -> IO (Result Document Text)
run (Document moduleName tests) = do
    let config = [ "repl", "--no-colors" ]
                    |> proc "elm"
                    |> setStdin createPipe
                    |> setStdout createPipe
                    |> setStderr createPipe

    p <- startProcess config

    -- Encodings
    hSetEncoding (getStdin p) utf8
    hSetEncoding (getStdout p) utf8
    hSetEncoding (getStderr p) utf8

    -- Disable buffering
    hSetBuffering (getStdin p) NoBuffering
    hSetBuffering (getStdout p) NoBuffering
    hSetBuffering (getStderr p) NoBuffering

    -- Ignore the first 3 lines
    Data.Text.IO.hGetLine (getStdout p)
    Data.Text.IO.hGetLine (getStdout p)
    Data.Text.IO.hGetLine (getStdout p)

    -- Import Document module
    Data.Text.IO.hPutStrLn
        (getStdin p)
        (Text.concat [ "import ", moduleName, " exposing (..)" ])

    -- Wait for a little while
    threadDelay (500 * 1000)

    -- Was the module imported successfully?
    err <- readFromHandle Nothing (getStderr p)

    -- Determine result
    result <-
        if Text.isInfixOf "-- UNKNOWN IMPORT --" err then
            "Could not find an Elm project."
                |> Err
                |> return
        else
            tests
                |> map (handleTest p)
                |> sequence
                |> map (Document moduleName .> Ok)

    -- Close up shop
    closeUpShop p

    -- The end
    return result



-- FULFILL


handleTest :: Process Handle Handle Handle -> Test -> IO Test
handleTest p test =
    let
        expectedOutSize =
            test
                |> expectedOutput
                |> Text.strip
                |> Text.length
    in
    case expectedOutSize of

        0 -> do
            let statement = Text.replace "\n" " " (input test)

            -- No expected output, so we assume the input is a preparing statement.
            Data.Text.IO.hPutStrLn (getStdin p) statement

            -- Possibly ignore next line
            if Text.isPrefixOf "import" statement then
                return ""
            else
                Data.Text.IO.hGetLine (getStdout p)

            -- Return
            return test { state = PrepareStatement }

        _ ->
            -- Normal test, carry on.
            fulfillTest p test


fulfillTest :: Process Handle Handle Handle -> Test -> IO Test
fulfillTest p test = do
    let inp         = Text.replace "\n" " " (input test)
    let outp        = Text.replace "\n" " " (expectedOutput test)
    let equation    = Text.concat [ "(==) (", inp, ") (", outp, ")" ]

    -- Pass it to the Elm REPL
    Data.Text.IO.hPutStrLn (getStdin p) equation

    -- Read from stderr
    err <- readFromHandle Nothing (getStderr p)

    -- Return error if present
    if Text.length err > 0 then
        return test { state = Error err }

    -- Otherwise get output from stdout
    else
        getOutput p test inp


getOutput :: Process Handle Handle Handle -> Test -> Text -> IO Test
getOutput p test inp = do
    line    <- Data.Text.IO.hGetLine (getStdout p)
    state   <- return (stateFromLine False line)

    case state of
        Error _ ->
            -- Ignore unparsable stuff (eg. debugging statements),
            -- and move on to the next line.
            getOutput p test inp

        Unequal _ -> do
            -- Execute the expected input and keep the result
            Data.Text.IO.hPutStrLn (getStdin p) inp
            line <- Data.Text.IO.hGetLine (getStdout p)
            return test { state = stateFromLine True line }

        _ ->
            return test { state = state }



-- ⚗️


closeUpShop :: Process Handle Handle Handle -> IO ()
closeUpShop p = do
    hClose (getStdin p)
    hClose (getStdout p)
    hClose (getStderr p)

    stopProcess p

    return ()


outputParser :: Parser [Char]
outputParser = do
    maybeSome (string "> ") -- ignore these
    manyTill anyChar (string " : " `andThen` manyTill anyChar eof)


readFromHandle :: Maybe Text -> Handle -> IO Text
readFromHandle maybePrevious handle = do
    -- Wait 750 ms at most for initial output,
    -- but no longer than 125 ms for following output.
    let waitTime = if isNothing maybePrevious then 750 else 125

    ready <- hWaitForInput handle waitTime

    -- If output is available
    if ready then do
        line <- Data.Text.IO.hGetLine handle

        case maybePrevious of
            Just previous   -> readFromHandle (Just <| Text.concat [ previous, "\n", line ]) handle
            Nothing         -> readFromHandle (Just <| line) handle

    -- If no output
    else
        return (fromMaybe "" maybePrevious)


stateFromLine :: Bool -> Text -> TestState
stateFromLine alwaysUnequal line =
    line
        |> Text.unpack
        |> (parseMaybe outputParser)
        |> map Text.pack
        |> maybe
             (Error "Cannot parse REPL output")
             (\o -> if not alwaysUnequal && o == "True" then Equal else Unequal o)
