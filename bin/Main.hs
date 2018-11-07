module Main where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Proofread (Result(..))
import Protolude
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
        Nothing         -> putError "Argument missing"



-- 📮


{-| Format contents from a file.
-}
formatFile :: [Char] -> IO ()
formatFile filePath = do
    contents <- readFile filePath

    let prefix = Text.pack (filePath ++ " → ")

    case traceShowId (Proofread.proofread contents) of
        Ok result   -> putSuccess (prefix <> "✔")
        Err err     -> putError (prefix <> "✘") >> putError err >> exitFailure



-- ⛳️


excludeFlags :: [[Char]] -> [[Char]]
excludeFlags = filter excludeFlag


excludeFlag :: [Char] -> Bool
excludeFlag ('-' : _) = False
excludeFlag _ = True



-- 🚦


putError :: Text -> IO ()
putError err =
    putStr ( color Red err )


putSuccess :: Text -> IO ()
putSuccess msg =
    putStr ( color Green msg )
