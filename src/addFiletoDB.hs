import qualified Data.ByteString as B
import Control.Monad.Reader (when)
import System.Console.GetOpt
import System.Environment
import System.Exit

import MBE.Store

data Opts = Opts {
          inFile    :: FilePath,
          dbFile    :: FilePath,
          showHelp  :: Bool
          }

defaults = Opts {
                inFile   = "/dev/stdin",
                dbFile   = "default.db",
                showHelp = False
                }

header = "Usage: addFiletoDB [opt... ]"

main :: IO ()
main = do
        (opts,_) <- getArgs >>= parseOpts

        when (showHelp opts) showUsageAndExit

        let input = inFile opts
        string <- if input == "/dev/stdin"
                      then B.getContents
                      else B.readFile input
        store (dbFile opts) string


options :: [OptDescr (Opts -> Opts)]
options = [
          Option "i" ["input"]
            (ReqArg (\arg opt -> opt {inFile = arg}) "INPUT") "Which file to process as input. (Defaults to stdin)",
          Option "d" ["database"]
            (ReqArg (\arg opt -> opt {dbFile = arg}) "DATABASE") "Which file to use as the database. (Defaults to default.db)",
          Option "h?" ["help"]
            (NoArg (\opt -> opt {showHelp = True})) "Show help and exit."
          ]

parseOpts :: [String] -> IO (Opts, [String])
parseOpts argv =
        case getOpt Permute options argv of
            (o,n,[]  ) -> return (foldl (flip id) defaults o, n)
            (_,_,errs) -> ioError (userError (concat errs ++ showUsage))

showUsage = usageInfo header options

showUsageAndExit = do
        putStrLn showUsage
        exitSuccess
