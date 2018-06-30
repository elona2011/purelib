module Encode2 where

import Prelude

import Data.Int (pow)
import Data.String (length)

codes2 :: String
codes2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

zipTimeToNum :: Int -> Array Int
zipTimeToNum t 
  | t < (pow (length codes2) 3) * 30 = [1]
  | otherwise = [2]