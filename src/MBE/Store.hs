module MBE.Store
    ( store
    ) where

import Data.List
import MDB.PMap
import System.Directory

punctuation :: String
punctuation = ",.; "

noPunctuation :: Char -> Bool
noPunctuation = flip notElem punctuation

groupByPunctuation :: String -> [String]
groupByPunctuation = groupBy (\a b -> all noPunctuation [a,b])

delSpaces :: [String] -> [String]
delSpaces l = l \\ (replicate (length l) " ")

checkDB :: FilePath -> IO Database
checkDB path = do
        dbExists <- doesFileExist path
        if dbExists
            then readDB path
            else createDB path

store :: FilePath -> String -> IO ()
store path s = do
        db <- checkDB path
        writeDB path $ inFunc db splitList
        where
            splitList = map (\x -> delSpaces $ groupByPunctuation x) lineList
            lineList  = lines s

inFunc :: Database -> [[Word]] -> Database
inFunc db (x:xs) = inFunc (wpMaker db ("^":x)) xs
inFunc db [] = db

wpMaker :: Database -> [Word] -> Database
wpMaker db (x:y:xs) = wpMaker (updateDB (x,y) db) (y:xs)
wpMaker db (x:[]) = updateDB (x,"$") db
wpMaker db [] = db
