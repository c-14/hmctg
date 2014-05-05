module MBE.Markov
    ( recChain
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as H
import qualified Numeric.Probability.Random as R
import System.Random

import MDB.LHashMap

space = C.singleton ' '
eol = C.singleton '$'
sol = C.singleton '^'

punctuation :: [B.ByteString]
punctuation = map C.singleton [',','.',';','!','?','$']

notPunctuation :: B.ByteString -> Bool
notPunctuation = flip notElem punctuation

recIOChain :: FilePath -> StdGen -> IO B.ByteString
recIOChain path g = do
        db <- importDB path
        let wList = listFunc g db []
        let rList = reverse wList
        return (foldl B.append B.empty rList)

recChain :: PDatabase -> StdGen -> B.ByteString
recChain db g = foldl B.append B.empty rList
    where
        rList = reverse wList
        wList = listFunc g db []

listFunc :: StdGen -> PDatabase -> [B.ByteString] -> [B.ByteString]
listFunc g db l = if null l then listFunc g2 db fstList else eolChck
        where
            eolChck = if head l == eol then tail l else f
            (g1,g2) = split g
            newList = if notPunctuation nxtWord then (nxtWord : space : l) else (nxtWord : l)
            nxtWord = getNextWord g1 db cw
            fstList = ((getNextWord g1 db sol) : l)
            cw      = if null l then sol else head l
            f       = listFunc g2 db newList

getNextWord :: StdGen -> PDatabase -> B.ByteString -> B.ByteString
getNextWord g db cw = R.runSeed g $ R.pick cwDist
    where
        cwDist  = db H.! cw
