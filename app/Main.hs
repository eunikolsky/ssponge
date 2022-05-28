module Main where

import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Version (showVersion)
import Paths_ssponge (version)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)

main :: IO ()
main = do
  filename <- getFilename
  maybeExistingContents <- readFileIfExists filename
  contents <- B.getContents
  writeContentsIfChanged filename maybeExistingContents contents

-- | Writes the file contents only if they have changed.
writeContentsIfChanged :: FilePath -> Maybe B.ByteString -> B.ByteString -> IO ()
writeContentsIfChanged filename maybeExistingContents contents =
  when shouldWriteFile $ B.writeFile filename contents

  where
    shouldWriteFile = case maybeExistingContents of
      Just existingContents -> contents /= existingContents
      Nothing -> True

-- | Reads the file contents if it exists.
readFileIfExists :: FilePath -> IO (Maybe B.ByteString)
readFileIfExists filename = do
  exists <- doesFileExist filename
  if exists
    then Just <$> B.readFile filename
    else pure Nothing

-- | Retrieves the filename as the single argument to the program.
-- Terminates program if it's not provided. Also supports printing
-- the program version when requested.
getFilename :: IO FilePath
getFilename = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn (showVersion version) >> exitSuccess
    [filename] -> return filename
    _ -> die "Usage: ssponge <filename>|--version"
