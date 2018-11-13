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
                |> map (fulfillTest p)
                |> sequence
                |> map (Document moduleName .> Ok)

    -- Close up shop
    closeUpShop p

    -- The end
    return result



-- FULFILL


fulfillTest :: Process Handle Handle Handle -> Test -> IO Test
fulfillTest p test = do
    let equation = Text.concat [ "(==) (", input test, ") (", expectedOutput test, ")" ]

    -- Pass it to the Elm REPL
    Data.Text.IO.hPutStrLn (getStdin p) equation

    -- Read from stderr
    err <- readFromHandle Nothing (getStderr p)

    -- Return error if present
    if Text.length err > 0 then
        return test { state = Error err }

    -- Otherwise get actual output
    else do
        line    <- Data.Text.IO.hGetLine (getStdout p)
        state   <- return (stateFromLine line)

        case state of
            Unequal _ -> do
                Data.Text.IO.hPutStrLn (getStdin p) (input test)
                line <- Data.Text.IO.hGetLine (getStdout p)
                return test { state = stateFromLine line }

            _ ->
                return test { state = state }


fulfillTest _ test = return test



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


stateFromLine :: Text -> TestState
stateFromLine line =
    line
        |> Text.unpack
        |> (parseMaybe outputParser)
        |> map Text.pack
        |> maybe
             (Error "Cannot parse REPL output")
             (\o -> if o == "True" then Equal else Unequal o)
