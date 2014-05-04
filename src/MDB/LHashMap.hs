module MDB.LHashMap
    ( importDB
    , pWriteDB
    , PDatabase
    ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import System.IO

import MDB.PHashMap

type Probability = Float
type WPPHMap = H.HashMap Word Probability
type PDatabase = H.HashMap Word WPPHMap

importDB :: FilePath -> IO PDatabase
importDB path = do
        db <- checkDB path
        return (calcProbability (mapCountConv db))

pWriteDB :: FilePath -> PDatabase -> IO ()
pWriteDB path db = withFile path WriteMode (\handle ->
                                        C.hPutStr handle . C.pack . show $ H.toList wpm)
                                        where
                                            wpm = H.map H.toList db

mapCountConv :: Database -> PDatabase
mapCountConv = H.map (H.map fromIntegral)

calcProbability :: PDatabase -> PDatabase
calcProbability = H.map probGet

probGet :: WPPHMap -> WPPHMap
probGet wp = H.map (/ sm) wp
    where
        sm = H.foldl' (+) 0 wp
