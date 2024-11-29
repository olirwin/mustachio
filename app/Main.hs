module Main where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)

import Parser.Template.CompleteTemplateParser (parseTemplate)
import Parser.Parser (evalParser, runParser)
import Parser.Data.JsonParser (parseJSON)
import Renderer.MoreRender (instTemplate)
import Program.Options (Options(..), defaultOptions, options)

import Program.FileChecker (ensureFileExists)
import Program.Errors (handleSyntaxError, missingInfoError)

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    case getOpt Permute options args of
      (actions, _, []) -> foldl (>>=) (return defaultOptions) actions
      (_, _, errs) -> do
        mapM_ putStrLn errs
        putStrLn $ usageInfo "Usage: program [OPTIONS]" options
        exitFailure

main :: IO ()
main = do
    opts <- parseArgs
    let maybeTemplate = optTemplate opts
        maybeData     = optData opts
        maybeOutput   = optOutput opts
        verbose       = optVerbose opts

    case (maybeTemplate, maybeData, maybeOutput) of
      (Just templateFile, Just dataFile, Just outputFile) -> do
          -- Check for input file existence
          ensureFileExists templateFile
          ensureFileExists dataFile
          -- Read the template file
          templateContent <- readFile templateFile
          let temp = runParser parseTemplate templateContent
          case temp of
            Just (t, "") -> do
                when verbose (putStrLn ("Parsed template: " ++ show t))
                jsonContent <- readFile dataFile
                let dta = runParser parseJSON jsonContent
                case dta of
                  Just (d, "") -> do
                    when verbose (putStrLn ("Parsed data: \n" ++ show d))
                    case instTemplate d t of
                      Nothing -> missingInfoError dataFile templateFile
                      Just str -> do
                        -- Write the output file
                        writeFile outputFile str
                        when verbose (putStrLn ("Output written to " ++ outputFile))
                  _            -> handleSyntaxError dataFile "data source parsing"
            _            -> handleSyntaxError templateFile "template parsing"
      _ -> do
        putStrLn "Error: Missing required arguments"
        putStrLn $ usageInfo "Usage: program [OPTIONS]" options
        exitFailure