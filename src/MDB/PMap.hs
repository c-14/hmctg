module MDB.PMap
    ( readDB
    , writeDB
    , createDB
    , updateDB
    , Database
    , Word
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO

type Count = Int
type Word = String
type WordSet = S.Set Word
type WordPair = (Word, Word)
type WPMap = M.Map WordPair Count
type Database = (WPMap, WordSet)

writeDB :: FilePath -> Database -> IO ()
writeDB path (wpm,ws) = withFile path WriteMode (\handle -> do
                                              hPrint handle $ M.toList wpm
                                              hPrint handle $ S.toList ws)

readDB :: FilePath -> IO Database
readDB path = withFile path ReadMode (\handle -> do
                                     wpm <- hGetLine handle
                                     ws <- hGetLine handle
                                     return (M.fromList $ read wpm, S.fromList $ read ws))

createDB :: FilePath -> IO Database
createDB path = withFile path WriteMode (\handle -> do
                                   hPrint handle $ M.toList wpp
                                   hPrint handle $ S.toList w
                                   return (wpp, w))
        where wpp = M.singleton ("^","$") 1
              w   = S.fromList ["^","$"]

updateDB :: WordPair -> Database -> Database
updateDB wp (wpm,ws) = (M.alter wpMapInsert wp wpm, wsInsert wp ws)

wpMapInsert :: Maybe Count -> Maybe Count
wpMapInsert a = case a of
                    Nothing -> Just 1
                    Just b -> Just (b + 1)

wsInsert :: WordPair -> WordSet -> WordSet
wsInsert (f,s) ws = S.insert s $ S.insert f ws
