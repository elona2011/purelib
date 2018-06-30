module DelPoint where

import Prelude

import Data.Array (deleteAt, elemIndex, head, length, sort, (!!), (..))
import Data.Maybe (Maybe(..))
import Math (abs, pow, sqrt)
import Partial.Unsafe ( unsafeCrashWith)

delPoint :: Array (Array Number) -> Array (Array Number)
delPoint arr = noMaybe(deleteAt i arr) where
  newArr = (arrangeArr150 >>> (map calTotal)) arr
  a = (sort >>> head >>> noMaybe) newArr
  i = noMaybe(elemIndex a newArr)

class NoMaybe a where
  noMaybe :: Maybe a -> a

instance noMaybeInt :: NoMaybe Int where
  noMaybe (Just i) = i 
  noMaybe _ = 0

instance noMaybeNumber :: NoMaybe Number where
  noMaybe (Just i) = i 
  noMaybe _ = 0.0

instance noMaybeArrayNumber :: NoMaybe (Array Number) where
  noMaybe (Just i) = i 
  noMaybe _ = []

instance noMaybeArrayArrayNumber :: NoMaybe (Array (Array Number)) where
  noMaybe (Just i) = i 
  noMaybe _ = [[]]

arrangeArr150 :: Array (Array Number) -> Array (Array ((Array Number)))
arrangeArr150 [] = [[[0.0]]]
arrangeArr150 [a] = [[[0.0]]]
arrangeArr150 [a,b] = [[[0.0]]]
arrangeArr150 x = do
  i <- 1 .. (length x -2)
  let a = indexNoMaybe x (i-1)
  let b = indexNoMaybe x (i)
  let c = indexNoMaybe x (i+1)
  [[a,b,c]]

indexNoMaybe ::  Array (Array Number) -> Int -> Array Number
indexNoMaybe arr i = noMaybe(arr !! i)

-- 计算该点和前后点的距离和时间差
calTotal :: Array (Array Number) -> Number
calTotal [a,b,c] = d1 * 10.0 + d2 * 10.0 + t where
  d1 = calDistanceArr [a,b,c] 
  d2 = calDistanceArr [a,b,c]
  t = calTimeArr [a,b,c]
calTotal _ = 0.0

calDistanceArr :: Array (Array Number) -> Number
calDistanceArr arr = calDistance x0 x1 y0 y1 where
  getElm = getArrElm arr
  x0 = getElm 0 0
  x1 = getElm 1 0
  y0 = getElm 1 1
  y1 = getElm 2 1

calDistance :: Number->Number->Number->Number -> Number
calDistance x0 x1 y0 y1 = sqrt ((pow (x0-x1) 2.0) + (pow (y0-y1) 2.0))

calTimeArr :: Array (Array Number) -> Number
calTimeArr arr = calTime t0 t1 t2 where
  getElm = getArrElm arr
  t0 = getElm 0 2
  t1 = getElm 1 2
  t2 = getElm 2 2

calTime :: Number -> Number -> Number -> Number
calTime t1 t2 t3 = (abs (t1-t2)) + (abs (t2-t3))

getArrElm :: Array (Array Number) ->Int->Int-> Number
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 0 0 = a
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 0 1 = b
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 0 2 = c
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 1 0 = d
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 1 1 = e
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 1 2 = f
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 2 0 = g
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 2 1 = h
getArrElm [[a,b,c],[d,e,f],[g,h,i]] 2 2 = i
getArrElm _ _ _ = unsafeCrashWith "f getArrElm get wrong arguments"