module Program.Errors (
    handleSyntaxError,
    missingInfoError
) where

import System.Exit (exitWith, ExitCode (..))

handleSyntaxError :: String -> String -> IO ()
handleSyntaxError file context = do
    putStrLn $ "Syntax error during " ++ context ++ " file: " ++ file
    exitWith (ExitFailure 2)

missingInfoError :: String -> String -> IO ()
missingInfoError jfile tfile = do
    putStrLn $ "Insufficient information in JSON data file " ++ jfile ++ " to fill template " ++ tfile
    exitWith (ExitFailure 3)
