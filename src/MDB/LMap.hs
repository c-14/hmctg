module MDB.LMap
    ( importDB
    , PDatabase
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import MDB.PMap

type WPPMap = M.Map WordPair Probability
type Probability = Float
type PDatabase = (WPPMap,WordSet)

-- Turns Database into Probability Database
importDB :: FilePath -> IO PDatabase
importDB path = do
        (wpm,ws) <- checkDB path
        let pList = makeProbability (mapCountConv wpm) ws
        return (pList,ws)

makeProbability :: WPPMap -> WordSet -> WPPMap
makeProbability = S.foldl' calcProbability

mapCountConv :: WPMap -> WPPMap
mapCountConv = M.map fromIntegral

calcProbability :: WPPMap -> Word -> WPPMap
calcProbability wpm wo = probMap
    where
        probMap = M.mapWithKey kCk wpm
        pMake a = a / sm
        sm   = getCount wpm wo
        kCk d e
                | fst d == wo = pMake e
                | otherwise   = e

-- Generate sum of times word w went somewhere.
getCount :: WPPMap -> Word -> Float
getCount wpm w = sum countList
    where keyList   = filter wordCheck $ M.keys wpm
          countList = map (flip (M.findWithDefault 0) wpm) keyList
          wordCheck k
                    | fst k == w = True
                    | otherwise  = False
