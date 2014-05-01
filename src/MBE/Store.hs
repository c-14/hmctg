module MBE.Store
    ( store
    ) where

import Data.List
import MDB.PMap

punctuation :: String
punctuation = ",.; "

noPunctuation :: Char -> Bool
noPunctuation = flip notElem punctuation

groupByPunctuation :: String -> [String]
groupByPunctuation = groupBy (\a b -> all noPunctuation [a,b])

delSpaces :: [String] -> [String]
delSpaces l = l \\ replicate (length l) " "

store :: FilePath -> String -> IO ()
store path s = do
        db <- checkDB path
        writeDB path $ inFunc db splitList
        where
            splitList = map (delSpaces . groupByPunctuation) lineList
            lineList  = lines s

inFunc :: Database -> [[Word]] -> Database
inFunc = foldl (\ db x -> wpMaker db ("^":x))

wpMaker :: Database -> [Word] -> Database
wpMaker db (x:y:xs) = wpMaker (updateDB (x,y) db) (y:xs)
wpMaker db (x:[]) = updateDB (x,"$") db
wpMaker db [] = db
