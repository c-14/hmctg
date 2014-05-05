module MDB.LHashMap
    ( importDB
    , pWriteDB
    , PDatabase
    ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import Numeric.Probability.Distribution
import System.IO

import MDB.PHashMap

type Probability = Float
type WPSHMap = H.HashMap Word Probability
type WPPHMap = T Probability Word
type SDatabase = H.HashMap Word WPSHMap
type PDatabase = H.HashMap Word WPPHMap

importDB :: FilePath -> IO PDatabase
importDB path = do
        db <- checkDB path
        let bs = calcProbability db
        return (probConv bs)

pWriteDB :: FilePath -> PDatabase -> IO ()
pWriteDB path db = withFile path WriteMode (\handle ->
                                        C.hPutStr handle . C.pack . show $ H.toList wpm)
                                        where
                                            wpm = db

calcProbability :: SDatabase -> SDatabase
calcProbability = H.map probGet

probGet :: WPSHMap -> WPSHMap
probGet wp = H.map (/ sm) wp
    where
        sm = H.foldl' (+) 0 wp

probConv :: SDatabase -> PDatabase
probConv = H.map (fromFreqs . H.toList)
