module Main where

import Control.Monad (unless)
import qualified Data.ByteString as B
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  filename <- getFilename
  maybeExistingContents <- readFileIfExists filename
  contents <- B.getContents
  writeContentsIfChanged filename maybeExistingContents contents

-- | Writes the file contents only if they have changed.
writeContentsIfChanged :: FilePath -> Maybe B.ByteString -> B.ByteString -> IO ()
writeContentsIfChanged filename maybeExistingContents contents =
  case maybeExistingContents of
    Just existingContents -> do
      unless (contents == existingContents) $
        B.writeFile filename contents

    Nothing -> B.writeFile filename contents

-- | Reads the file contents if it exists.
readFileIfExists :: FilePath -> IO (Maybe B.ByteString)
readFileIfExists filename = do
  exists <- doesFileExist filename
  if exists
    then Just <$> B.readFile filename
    else pure Nothing

-- | Retrieves the filename as the single argument to the program.
-- Terminates program if it's not provided.
getFilename :: IO FilePath
getFilename = do
  args <- getArgs
  case args of
    [filename] -> return filename
    _ -> die "Usage: ssponge <filename>"
