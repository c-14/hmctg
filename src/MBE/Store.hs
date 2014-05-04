module MBE.Store
    ( store
    ) where

import Data.List
import MDB.PMap
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Word as W

punctuation :: B.ByteString
punctuation = C.pack ",.;\t!? "

noPunctuation :: W.Word8 -> Bool
noPunctuation = flip B.notElem punctuation

groupByPunctuation :: B.ByteString -> [B.ByteString]
groupByPunctuation = B.groupBy (\a b -> B.all noPunctuation $ B.pack [a,b])

delSpaces :: [B.ByteString] -> [B.ByteString]
delSpaces l = l \\ replicate (length l) (C.singleton ' ')

store :: FilePath -> B.ByteString -> IO ()
store path s = do
        db <- checkDB path
        writeDB path $ inFunc db splitList
        where
            splitList = map (delSpaces . groupByPunctuation) lineList
            lineList  = C.lines s

inFunc :: Database -> [[Word]] -> Database
inFunc = foldl (\ db x -> wpMaker db (C.singleton '^':x))

wpMaker :: Database -> [Word] -> Database
wpMaker db (x:y:xs) = wpMaker (updateDB (x,y) db) (y:xs)
wpMaker db (x:[]) = updateDB (x,C.singleton '$') db
wpMaker db [] = db
