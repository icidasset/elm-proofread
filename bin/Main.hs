module Main where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Flow
import Proofread
import Protolude hiding (state)
import System.Console.Pretty
import System.Console.CmdArgs

import qualified Data.List as List
import qualified Data.Text as Text


-- 🌳


data Flags = Flags
    { filePath :: Maybe [Char]
    , showAllErrors :: Bool
    }
    deriving (Data, Show, Typeable)


flagsConfig = Flags
    { filePath = Nothing &= args &= typFile

    --
    , showAllErrors = False
        &= name "all-errors"
        &= explicit
        &= help "Show all errors instead of one"
    }

    -- Summary
    ----------
    &= summary "\nProofread your Elm files.\n\
               \See https://github.com/icidasset/elm-proofread for examples."

    -- Program
    ----------
    &= program "elm-proofread"



-- 🍯


main :: IO ()
main = do
    flags <- cmdArgs flagsConfig

    -- Proofread!
    case filePath flags of
        Just filePath   -> processFile filePath flags
        Nothing         -> processStdin flags



-- 📮


{-| Proofread a single file.
-}
processFile :: [Char] -> Flags -> IO ()
processFile filePath flags = do
    putStrLn (Text.concat [ "Proofreading ", Text.pack filePath ])

    contents    <- readFile filePath
    result      <- Proofread.proofread contents

    handleResult result flags


{-| Proofread stdin.
-}
processStdin :: Flags -> IO ()
processStdin flags = do
    putStrLn ("Proofreading stdin" :: Text)

    contents    <- getContents
    result      <- Proofread.proofread contents

    handleResult result flags



-- 📮  ~  Generic


handleResult :: Result Document Text -> Flags -> IO ()
handleResult (Err err) _ = putErrorLn err >> exitFailure
handleResult (Ok (Document _ tests)) flags = do
    _ <- tests
        |> map renderTest
        |> sequence

    -- Render first error, hide others (unless all-errors flag is provided)
    -- Does nothing if no errors are present
    _ <- tests
        |> List.filter
            (\t ->
                case state t of
                    Error _     -> True
                    Unequal _   -> True
                    _           -> False
            )
        |> (
            if showAllErrors flags then
                identity

            else
                List.take 1
        )
        |> map renderTestError
        |> sequence

    -- Did all tests pass?
    let passedTests = filter (state .> (==) Equal) tests
    let actualTests = filter (state .> (/=) PrepareStatement) tests

    -- Render success message if appropiate
    if length passedTests == length actualTests then
        putSuccess ("\n\nAll tests passed!" :: Text) >> exitSuccess

    -- Otherwise
    else
        putStr ("\n\n" :: Text) >> exitFailure



-- ⛳️


excludeFlags :: [[Char]] -> [[Char]]
excludeFlags = filter excludeFlag


excludeFlag :: [Char] -> Bool
excludeFlag ('-' : _) = False
excludeFlag _ = True



-- ⚗️


renderTest :: Test -> IO ()
renderTest (Test { state }) =
    case state of
        NotFulfilled ->
            return ()

        PrepareStatement ->
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

        PrepareStatement ->
            return ()

        Equal ->
            return ()

        Unequal actualOutput ->
            let
                inputLines =
                    Text.lines input

                formattedInput =
                    inputLines
                        |> List.drop 1
                        |> List.map (Text.append "    ")
                        |> (++) (List.take 1 inputLines)
                        |> Text.unlines
            in
            [ "\n\n\n"
            , separator
            , "\n\n"
            , "The test found on line "
            , show lineNumber
            , " failed. You said\n\n\n    "
            , formattedInput
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
