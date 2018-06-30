module Utils where

import Prelude

import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Data.Array ( length )
import DelPoint (delPoint)
import Encode (zipXY, zipn)

joinInfo :: Array String -> String
joinInfo = map (replaceAll (Pattern "~") (Replacement "-")) >>> joinWith "~"

-- Int 上限
addPoint :: Int -> Array (Array Number) -> Array Number -> Array (Array Number)
addPoint n arr point | length arr >= n = delPoint(arr <> [point])
  | otherwise = arr <> [point]

-- 
encodePoint :: Array (Array Int) -> String
encodePoint = map ec >>> joinWith "" where
  ec :: Array Int -> String
  ec [a,b,c] = zipXY a <> zipXY b <> zipn 3 c 
  ec _ = "encodePoint error!"