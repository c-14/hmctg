import qualified Data.ByteString.Char8 as C
import Control.Monad (forever,when)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import System.IO

import MBE.Markov
import MDB.LHashMap

data Opts = Opts {
          dbFile    :: FilePath,
          outFile   :: FilePath,
          showHelp  :: Bool
          }

defaults = Opts {
                dbFile      = "default.db",
                outFile     = "/dev/stdout",
                showHelp    = False
                }

header = "Usage: hmctg [opt... ]"

main :: IO ()
main = do
        (opts,_) <- getArgs >>= parseOpts

        when (showHelp opts) showUsageAndExit

        db <- importDB (dbFile opts)
        let output = outFile opts
        if output == "/dev/stdout"
            then forever $ do
                gen <- newStdGen
                C.putStrLn $ recChain db gen
            else withFile output WriteMode (\handle -> forever $ do
                                           gen <- newStdGen
                                           C.hPutStrLn handle $ recChain db gen)

options :: [OptDescr (Opts -> Opts)]
options = [
          Option "d" ["database"]
            (ReqArg (\arg opt -> opt {dbFile = arg}) "INPUT") "Which database to process. (Defaults to default.db)",
          Option "o" ["output"]
            (ReqArg (\arg opt -> opt {outFile = arg}) "OUTPUT") "Which file to output to. (Defaults to stdout)",
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
