module Program.FileChecker (
    ensureFileExists
) where

import System.Directory (doesFileExist)
import System.Exit (exitWith, ExitCode(..))

ensureFileExists :: FilePath -> IO ()
ensureFileExists file = do
    exists <- doesFileExist file
    if not exists
        then do
            putStrLn $ "Error: File does not exist - " ++ file
            exitWith (ExitFailure 1)  -- Exit code 1 for missing file
        else return ()
