module MBE.Store
    ( store
    ) where

import Data.List
import Database.HDBC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Word as W

import MDB.PDB

punctuation :: B.ByteString
punctuation = C.pack ",.;!? "

noPunctuation :: W.Word8 -> Bool
noPunctuation = flip B.notElem punctuation

groupByPunctuation :: B.ByteString -> [B.ByteString]
groupByPunctuation = B.groupBy (\a b -> B.all noPunctuation $ B.pack [a,b])

delSpaces :: [B.ByteString] -> [B.ByteString]
delSpaces l = l \\ replicate (length l) (C.singleton ' ')

store :: FilePath -> B.ByteString -> IO ()
store path s = do
        db <- checkDB path
        insertSQL <- prepare db "INSERT OR IGNORE INTO WordPairs (start, next, count, prob) VALUES ( ?, ?, 1, 1.0)"
        incrmtSQL <- prepare db "UPDATE WordPairs SET count = count + 1, prob = CAST(count + 1 AS REAL) / ((SELECT SUM(count) FROM WordPairs WHERE start = ?) + 1) WHERE start = ? AND next = ?"

        executeMany insertSQL $ map (wordPairToSql False) insertList
        commit db
        executeMany incrmtSQL $ map (wordPairToSql True) wordPairList
        commit db
        disconnect db
        where
            insertList = nub wordPairList
            wordPairList = flatten splitList
            flatten = foldl createPairs []
            splitList = map (delSpaces . groupByPunctuation) lineList
            lineList  = C.lines s

wordPairToSql :: Bool -> WordPair -> [SqlValue]
wordPairToSql inc wp
    | inc == True = map toSql [fst wp, fst wp, snd wp]
    | otherwise   = map toSql [fst wp, snd wp]

createPairs :: [WordPair] -> [Word] -> [WordPair]
createPairs wp (y:z:zs) = createPairs ((y, z):wp) (z:zs)
createPairs wp (x:[]) = createPairs ((x, C.singleton '$'):wp) []
createPairs wp [] = wp
