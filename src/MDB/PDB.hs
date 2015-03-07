module MDB.PDB
    ( checkDB
    , Count
    , Word
    , WordPair
    , Database
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.IO
import System.Directory
import Database.HDBC
import Database.HDBC.Sqlite3

type Count = Int
type Probability = Double
type Word = B.ByteString
type WordPair = (Word, Word)
type Database = Connection

checkDB :: FilePath -> IO Database
checkDB path = do
    db <- connectSqlite3 path
    tables <- getTables db
    if "WordPairs" `elem` tables
        then return db
        else createDB db

createDB :: Database -> IO Database
createDB db = do
    run db "CREATE TABLE WordPairs (start TEXT NOT NULL, next TEXT NOT NULL, count INT NOT NULL, prob REAL NOT NULL)" []
    run db "INSERT INTO WordPairs (start, next, count, prob) VALUES ( ?, ?, ?, ?)" [toSql $ C.singleton '^', toSql $ C.singleton '$', toSql (1 :: Count), toSql (1.0 :: Probability)]
    commit db
    return db
