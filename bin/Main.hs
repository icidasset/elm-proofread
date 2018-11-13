module Main where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Flow
import Proofread
import Protolude hiding (state)
import System.Console.Pretty
import System.Environment (getArgs)

import qualified Data.Text as Text
import qualified Proofread


-- 🍯


main :: IO ()
main = do
    args <- getArgs

    -- Fun w/ flags
    let maybeFilePath = listToMaybe (excludeFlags args)

    -- Format!
    case maybeFilePath of
        Just filePath   -> formatFile filePath
        Nothing         -> putErrorLn "Argument missing"



-- 📮


{-| Format contents from a file.
-}
formatFile :: [Char] -> IO ()
formatFile filePath = do
    putStrLn (Text.concat [ "Proofreading ", Text.pack filePath ])

    contents    <- readFile filePath
    result      <- Proofread.proofread contents

    case result of
        Ok (Document _ tests) -> do
            tests
                |> map renderTest
                |> sequence

            tests
                |> map renderTestError
                |> sequence

            -- Did all tests pass?
            let passedTests = filter (state .> (==) Equal) tests

            -- Render success message if appropiate
            if length passedTests == length tests then
                putSuccess ("\n\nAll tests passed!" :: Text)

            -- Otherwise
            else
                putStr ("\n\n" :: Text)

        Err err ->
            putErrorLn err >> exitFailure



-- ⛳️


excludeFlags :: [[Char]] -> [[Char]]
excludeFlags = filter excludeFlag


excludeFlag :: [Char] -> Bool
excludeFlag ('-' : _) = False
excludeFlag _ = True



-- ⚗️


renderTest :: Test -> IO ()
renderTest (Test { input, state }) =
    case state of
        NotFulfilled ->
            return ()

        Equal ->
            putStr ("." :: Text)

        Unequal _ ->
            putError "."

        Error _ ->
            putError "."


renderTestError :: Test -> IO ()
renderTestError (Test { expectedOutput, input, lineNumber, state }) =
    case state of
        NotFulfilled ->
            return ()

        Equal ->
            return ()

        Unequal actualOutput ->
            [ "\n\n\n"
            , separator
            , "\n\n"
            , "The test found on line "
            , show lineNumber
            , " failed. You said\n\n\n    "
            , input
            , "\n\n\nwas going to be equal to\n\n\n    "
            , expectedOutput
            , "\n\n\nbut it isn't. Instead got\n\n\n    "
            , actualOutput
            ]
                |> Text.concat
                |> putError

        Error err ->
            [ "\n\n\n"
            , separator
            , "\n\n"
            , "The test found on line "
            , show lineNumber
            , " failed. I got the following error message.\n\n"
            , err
            ]
                |> Text.concat
                |> putError


putError :: Text -> IO ()
putError err =
    putStr ( color Red err )


putErrorLn :: Text -> IO ()
putErrorLn err =
    putError (Text.append err "\n")


putSuccess :: Text -> IO ()
putSuccess msg =
    putStr ( color Green msg )


putSuccessLn :: Text -> IO ()
putSuccessLn msg =
    putSuccess (Text.append msg "\n")


separator :: Text
separator =
    "============================================================================"
