module Program.Options (
  Options (..),
  defaultOptions,
  options
) where
  
import System.Console.GetOpt (OptDescr (..), ArgDescr (..), usageInfo)
import System.Exit (exitSuccess)

-- | Data type declaration of options
data Options = Options { 
    optTemplate :: Maybe FilePath, 
    optData     :: Maybe FilePath,
    optOutput   :: Maybe FilePath,
    optVerbose  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options { 
    optTemplate = Nothing,
    optData     = Nothing,
    optOutput   = Nothing,
    optVerbose  = False
  }

options :: [OptDescr (Options -> IO Options)]
options = [ 
  Option ['t'] ["template"] (
    ReqArg (\f opts -> return opts { optTemplate = Just f }) "FILE"
  ) "Path to the template file",
  Option ['d'] ["data"] (
    ReqArg (\f opts -> return opts { optData = Just f }) "FILE"
  ) "Path to the data file (JSON)",
  Option ['o'] ["output"] (
    ReqArg (\f opts -> return opts { optOutput = Just f }) "FILE"
    ) "Path to the output file",
  Option ['h'] ["help"] (
    NoArg (\_ -> do 
      putStrLn $ usageInfo "Usage: mustachio [OPTIONS]" options
      exitSuccess
      )) "Display this help message",
  Option ['v'] ["verbose"] (
    NoArg (\opts -> return opts { optVerbose = True })
    ) "Enable verbose mode"
  ]
