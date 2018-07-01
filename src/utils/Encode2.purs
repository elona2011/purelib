module Encode2 where

import Prelude

import Data.Int (floor, pow, toNumber)
import Data.List (List(..), length, singleton, snoc, modifyAt, (:))
import Data.Maybe (fromJust)
import Data.String (length) as String
import Data.String.CodeUnits (charAt, singleton) as String
import Math ((%))
import Partial.Unsafe (unsafePartial)

class Codes where
  codes :: String
  
instance codes0 :: Codes where
  codes = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_"

class Len where
  len :: Int
  
instance len0 :: Codes => Len where
  len = String.length codes

data Value = Interval Number

class NonNeg a where
  nonNeg :: a -> Int

instance nonNegInt :: NonNeg Value where
  nonNeg (Interval t) 
    | t < 0.0 = 0
    | otherwise = floor t
  
class Zip a where
  zip :: a -> String

instance zipTime :: (Codes, NonNeg Value) => Zip Value where
  zip (Interval t) = arrEncode (zipTimeToNum (nonNeg (Interval t)) (String.length codes))

exZipTime :: Value -> String
exZipTime = zip 

arrEncode :: Codes => List Int -> String
arrEncode Nil = "" 
arrEncode (Cons x xs) = String.singleton (unsafePartial (fromJust $ String.charAt x codes)) <> arrEncode xs
  
data Steps = Step0 | Step1 | Step2

class TimeStep a where
  timeStep :: a -> Int

instance timeStep0 :: Len => TimeStep Steps where
  timeStep Step0 = (pow len 3) * 30
  timeStep Step1 = timeStep Step0 + (pow len 3) * 20 * 10
  timeStep Step2 = timeStep Step1 + (pow len 3) * 14 * 50

zipTimeToNum :: Int -> Int -> List Int
zipTimeToNum t codesLen
  | t < timeStep Step0 = editListLen (getParams t codesLen) 4 0
  | t < timeStep Step1 = editListLen (getParams ((t-timeStep Step0)/10) codesLen ) 4 30
  | t < timeStep Step2 = editListLen (getParams ((t-timeStep Step1)/50) codesLen ) 4 50
  | otherwise = (codesLen-1):(codesLen-1):(codesLen-1):(codesLen-1):Nil

getParams :: Int -> Int -> List Int
getParams num codesLen 
  | num >= codesLen = Cons (floor ((toNumber num) % (toNumber codesLen))) (getParams (num / codesLen) codesLen ) 
  | otherwise = singleton (num )

editListLen :: List Int -> Int -> Int -> List Int
editListLen a size offset
  | length a < size = editListLen (snoc a 0) size offset
  | otherwise = unsafePartial $ fromJust $ modifyAt (length a - 1) (\n -> n+offset) a