module MDB.PHashMap
    ( checkDB
    , writeDB
    , updateDB
    , Count
    , Word
    , WPHMap
    , Database
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import System.IO
import System.Directory

type Count = Float
type Word = B.ByteString
type WordPair = (Word, Word)
type WPHMap = H.HashMap Word Count
type Database = H.HashMap Word WPHMap

writeDB :: FilePath -> Database -> IO ()
writeDB path db = withFile path WriteMode (\handle ->
                                          C.hPutStr handle . C.pack . show $ H.toList wpm)
                                          where
                                              wpm = H.map H.toList db

readDB :: FilePath -> IO Database
readDB path = do
        file <- B.readFile path
        return (H.map H.fromList . H.fromList . read $ C.unpack file)

checkDB :: FilePath -> IO Database
checkDB path = do
        dbExists <- doesFileExist path
        if dbExists
            then readDB path
            else createDB path

createDB :: FilePath -> IO Database
createDB path = do
        writeDB path db
        return db
        where
            db  = H.singleton (C.singleton '^') sdb
            sdb = H.singleton (C.singleton '$') 1

updateDB :: WordPair -> Database -> Database
updateDB (w1,w2) db = H.insert w1 (insertWord w2 wm) db
    where
        wm  = H.lookupDefault def w1 db
        def = H.singleton w2 0

insertWord :: Word -> WPHMap -> WPHMap
insertWord w = H.insertWith (+) w 1
