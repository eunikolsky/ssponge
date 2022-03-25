module Main where

import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  filename <- getFilename
  contents <- B.getContents
  B.writeFile filename contents

-- | Retrieves the filename as the single argument to the program.
-- Terminates program if it's not provided.
getFilename :: IO FilePath
getFilename = do
  args <- getArgs
  case args of
    [filename] -> return filename
    _ -> die "Usage: ssponge <filename>"
