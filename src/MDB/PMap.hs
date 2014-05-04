module MDB.PMap
    ( checkDB
    , writeDB
    , createDB
    , updateDB
    , Count
    , Database
    , Word
    , WordPair
    , WordSet
    , WPMap
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import System.Directory

type Count = Int
type Word = B.ByteString
type WordSet = S.Set Word
type WordPair = (Word, Word)
type WPMap = M.Map WordPair Count
type Database = (WPMap, WordSet)

writeDB :: FilePath -> Database -> IO ()
writeDB path (wpm,ws) = withFile path WriteMode (\handle -> do
                                              C.hPutStrLn handle . C.pack $ show wpm
                                              C.hPutStrLn handle . C.pack $ show ws)

readDB :: FilePath -> IO Database
readDB path = do
        file <- B.readFile path
        let line = C.lines file
        return (read . C.unpack $ head line, read . C.unpack $ last line)

checkDB :: FilePath -> IO Database
checkDB path = do
        dbExists <- doesFileExist path
        if dbExists
            then readDB path
            else createDB path

createDB :: FilePath -> IO Database
createDB path = withFile path WriteMode (\handle -> do
                                   C.hPutStrLn handle . C.pack $ show wpp
                                   C.hPutStrLn handle . C.pack $ show w
                                   return (wpp, w))
        where wpp = M.singleton (C.singleton '^',C.singleton '$') 1
              w   = S.fromList [C.singleton '^',C.singleton '$']

updateDB :: WordPair -> Database -> Database
updateDB wp (wpm,ws) = (M.alter wpMapInsert wp wpm, wsInsert wp ws)

wpMapInsert :: Maybe Count -> Maybe Count
wpMapInsert a = case a of
                    Nothing -> Just 1
                    Just b -> Just (b + 1)

wsInsert :: WordPair -> WordSet -> WordSet
wsInsert (f,s) ws = S.insert s $ S.insert f ws
