module MDB.LMap
    ( importDB
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import MDB.PMap

type WordList = [Word]
type WPPMap = M.Map WordPair Probability
type Probability = Float
type PDatabase = (WPPMap,WordSet)

-- Turns Database into Probability Database
importDB :: FilePath -> IO PDatabase
importDB path = do
        (wpm,ws) <- checkDB path
        let wl = S.toList ws
        let shit = makeProbability (mapCountConv wpm) wl wl
        return (shit,ws)

makeProbability :: WPPMap -> WordList -> WordList -> WPPMap
makeProbability wpm wl (w:ws) = makeProbability (calcProbability wpm w wl) wl ws
makeProbability wpm _ [] = wpm

mapCountConv :: WPMap -> WPPMap
mapCountConv = M.map fromIntegral

calcProbability :: WPPMap -> Word -> WordList -> WPPMap
calcProbability wpm wo wl = probMap
    where
        probMap = M.mapWithKey kCk wpm
        pMake a = a / sm
        sm      = sum $ map fst cList
        cList   = getCount wpm wo wl
        kCk d e
                | fst d == wo = pMake e
                | otherwise   = e

-- Create list with number of times word w went somewhere.
getCount :: WPPMap -> Word -> WordList -> [(Probability,WordPair)]
getCount wpm w wl = map rmJust shrunk
    where fullList  = zip (repeat w) wl
          countList = map (`M.lookup` wpm) fullList
          zipped    = zip countList fullList
          shrunk    = filter cntTest zipped
          cntTest b = case fst b of
                            Nothing -> False
                            Just _ -> True
          rmJust b  = case fst b of
                             Nothing -> (0.0,snd b)
                             Just c -> (c,snd b)
