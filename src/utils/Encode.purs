module Encode where

import Data.String
import Prelude

import Data.Array as Arr 
import Data.Int (toNumber, floor, pow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt, length, singleton)
import Math ((%))

codes :: String
codes = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_"

len :: Int
len = length codes

zipXY :: Int -> String
zipXY n = zipn 2 $ floor $ toNumber $ n / 2

zipn :: Int -> Int -> String
zipn size n | n < pow len size = getCodeByParams size n 
  | otherwise = getCodeMax size 

getCodeMax :: Int -> String
getCodeMax 1 = getCode (len - 1)
getCodeMax size = (getCodeMax (size - 1)) <> getCode (len - 1)

getCodeByParams :: Int -> Int -> String
getCodeByParams size n = getCodeByParamsSize size (size - 1) n

getCodeByParamsSize :: Int -> Int -> Int -> String
getCodeByParamsSize size 0 n = getCode $ getEl 0 $ getFixParams size n
getCodeByParamsSize size b n = (getCodeByParamsSize size (b - 1) n) <> (getCode $ getEl b $ getFixParams size n)

getFixParams :: Int -> Int -> Array Int 
getFixParams size n = (getParams size >>> editArrLen size) n

getParams :: Int -> Int -> Array Int
getParams size n | n < len = [n]
  | otherwise = [floor ((toNumber n) % toNumber len)] <> getParams size (floor (toNumber n / toNumber len))

editArrLen :: Int -> Array Int -> Array Int 
editArrLen size arr | Arr.length arr < size = editArrLen size (arr <> [0])
  | otherwise = arr

getCode :: Int -> String
getCode i = case charAt i codes of 
  Nothing -> "0"
  Just r -> singleton r

getEl :: Int -> Array Int -> Int 
getEl i arr = case (arr Arr.!! i) of 
  Nothing -> 0
  Just r -> r