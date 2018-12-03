{-

RUNNER
======

-}
module Proofread.Runner where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Combinators
import Flow
import Proofread.Parser.Common
import Proofread.Parser.Types (Parser)
import Proofread.Types
import Protolude hiding (handle, moduleName, or, state)
import System.IO hiding (print)
import System.Process.Typed
import System.Timeout (timeout)
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
    _ <- Data.Text.IO.hGetLine (getStdout p)
    _ <- Data.Text.IO.hGetLine (getStdout p)
    _ <- Data.Text.IO.hGetLine (getStdout p)

    -- Import Document module
    Data.Text.IO.hPutStrLn
        (getStdin p)
        (Text.concat [ "import ", moduleName, " exposing (..)" ])

    -- Wait for a little while
    threadDelay (500 * 1000)

    -- Was the module imported successfully?
    err <- readFromHandle errorDetection (getStderr p)

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



-- SEGMENT


handleTest :: Process Handle Handle Handle -> Test -> IO Test
handleTest p test =
    case state test of

        PrepareStatement -> do
            let statement = Text.replace "\n" " " (input test)

            -- No expected output, so we assume the input is a preparing statement.
            Data.Text.IO.hPutStrLn (getStdin p) statement

            -- Possibly ignore next line
            _ <- if Text.isPrefixOf "import" statement then
                return ""
            else
                Data.Text.IO.hGetLine (getStdout p)

            -- Return
            return test

        _ ->
            -- Normal test, carry on.
            fulfillTest p test


fulfillTest :: Process Handle Handle Handle -> Test -> IO Test
fulfillTest p test = do
    let inp         = Text.replace "\n" " " (input test)
    let outp        = Text.replace "\n" " " (Text.replace "\n    " "\n" <| expectedOutput test)
    let equation    = Text.concat [ "(==) (", inp, ") (", outp, ")" ]

    -- Pass it to the Elm REPL
    Data.Text.IO.hPutStrLn (getStdin p) equation

    -- Read from stderr
    err <- readFromHandle errorDetection (getStderr p)

    -- Return error if present
    if Text.length err > 0 then
        return test { state = Error err }

    -- Otherwise get output from stdout
    else
        getOutput p test inp



-- OUTPUT


getOutput :: Process Handle Handle Handle -> Test -> Text -> IO Test
getOutput p test inp = do
    lines   <- readFromHandle inputDetection (getStdout p)
    state   <- return (deriveState False lines)

    case state of
        Unequal _ -> do
            -- Execute the expected input and keep the result
            Data.Text.IO.hPutStrLn (getStdin p) inp
            lines_ <- readFromHandle inputDetection (getStdout p)
            return test { state = deriveState True lines_ }

        _ ->
            return test { state = state }


deriveState :: Bool -> Text -> TestState
deriveState alwaysUnequal lines =
    maybe
        (Error "Cannot parse REPL output")
        (\o -> if alwaysUnequal || o /= "True" then Unequal o else Equal)
        (lines
            |> Text.replace "\n    : " " : "
            |> Text.lines
            |> lastMay
            |> fromMaybe ""
            |> parseOutput
        )


outputParser :: Parser [Char]
outputParser = do
    _ <- maybeSome (string "> ") -- ignore these
    manyTill anyChar (string " : " `andThen` manyTill anyChar eof)


parseOutput :: Text -> Maybe Text
parseOutput line =
    line
        |> Text.unpack
        |> (parseMaybe outputParser)
        |> map Text.pack



-- READING (GENERIC)


readFromHandle :: (Detector, Options) -> Handle -> IO Text
readFromHandle =
    readFromHandle_ Nothing


readFromHandle_ :: Maybe Text -> (Detector, Options) -> Handle -> IO Text
readFromHandle_  maybeAccumulated (lastLineDetector, options) handle = do
    let rounds      = checkingRounds options
    let fraction    = (timeToWait options * 1000) / rounds

    -- Get the line from the handle in a new thread
    mVar        <- newEmptyMVar
    tId         <- forkIO (Data.Text.IO.hGetLine handle >>= putMVar mVar)

    -- Check every x milliseconds if we have the line yet
    let loop = \n -> do
                   result <- timeout (round fraction) (takeMVar mVar)

                   case result of
                       Nothing -> if n < rounds then loop (n + 1) else return Nothing
                       Just l  -> return (Just l)

    -- Start loop
    maybeLine <- loop 1

    -- Kill thread when done
    killThread tId

    -- Moving on,
    -- either continue or stop.
    case maybeLine of

        Just line ->
            concatReadings
                (if lastLineDetector line then
                    return
                else
                    \r -> readFromHandle_ (Just r) (lastLineDetector, options) handle
                )
                maybeAccumulated
                line

        Nothing ->
            return (fromMaybe "" maybeAccumulated)


concatReadings :: (Text -> IO Text) -> Maybe Text -> Text -> IO Text
concatReadings c m line =
    case m of
        Just accumulated ->
            c <| Text.concat [ accumulated, "\n", line ]

        Nothing ->
            c <| line



-- (LAST LINE) DETECTORS


type Detector = Text -> Bool
data Options = Options { checkingRounds :: Float, timeToWait :: Float }


errorDetection :: (Detector, Options)
errorDetection =
    ( \_ -> False
    , Options { checkingRounds = 10, timeToWait = 500 }
    )


inputDetection :: (Detector, Options)
inputDetection =
    ( parseOutput .> isJust
    , Options { checkingRounds = 20, timeToWait = 2500 }
    )



-- ⚗️


closeUpShop :: Process Handle Handle Handle -> IO ()
closeUpShop p = do
    hClose (getStdin p)
    hClose (getStdout p)
    hClose (getStderr p)

    stopProcess p

    return ()
