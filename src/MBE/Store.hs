module MBE.Store
    ( store
    ) where

import Control.Monad
import Data.List
import Database.HDBC
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
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
        insertSQL <- prepare db "INSERT OR IGNORE INTO WordPairs (start, next, count, prob) VALUES ( ?, ?, 0, 0.0)"
        icountSQL <- prepare db "UPDATE WordPairs SET count = count + 1 WHERE start = ? AND next = ?"
        iprobaSQL <- prepare db "UPDATE WordPairs SET prob = CAST(count AS REAL) / (SELECT SUM(count) FROM WordPairs WHERE start = ?) WHERE start = ?"
        existsSQL <- prepare db "SELECT count from WordPairs WHERE start = ? AND next = ?"
        -- val <- filterM (notInDB existsSQL) (map wordPairToSql $ head wordPairList)
        -- print val
        mapM_ (flip (>>) (commit db) . (flip (>>=) (sqlExec . executeMany insertSQL) . (filterM $ sqlExec . notInDB existsSQL) . map wordPairToSql)) wordPairList
        -- commit db
        -- executeMany icountSQL $ map wordPairToSql wordPairList
        -- commit db
        -- executeMany iprobaSQL $ map (replicate 2 . toSql . fst) insertList
        -- commit db
        -- disconnect db
        where
            -- insertList = map (filterM $ notInDB existsSQL) wordPairList
            wordPairList = semiFlatten $ pairize splitList
            pairize = map $ createPairs []
            splitList = map (delSpaces . groupByPunctuation) lineList
            lineList  = C.lines s

notInDB :: Statement -> [SqlValue] -> IO Bool
notInDB s wp = do
    execute s wp
    val <- fetchRow s
    maybe (return True) (\x -> return False) val

semiFlatten :: [[WordPair]] -> [[WordPair]]
semiFlatten list 
            | null list = []
            | otherwise = (:) (concat . fst $ split) $ semiFlatten $ snd split
            where
                split = splitAt 5 list

wordPairToSql :: WordPair -> [SqlValue]
wordPairToSql wp = map toSql [fst wp, snd wp]

createPairs :: [WordPair] -> [Word] -> [WordPair]
createPairs wp (y:z:zs) = createPairs ((y, z):wp) (z:zs)
createPairs wp (x:[]) = createPairs ((x, C.singleton '$'):wp) []
createPairs wp [] = wp
