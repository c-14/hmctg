module MDB.PDB
    ( checkDB
    , sqlExec
    , Count
    , Word
    , WordPair
    , Database
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
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

sqlExec :: IO a -> IO a
sqlExec a = catchSql a (\x -> handleError a x)

handleError :: IO a -> SqlError -> IO a
handleError a e 
                | conv e == busy = a
                | conv e == lock = a
                | otherwise      = fail ("SQL error: " ++ show e)
                    where
                        conv = seNativeError
                        busy = sqlite_BUSY
                        lock = sqlite_LOCKED
