module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Class.Console (log)
import Encode2 (Value(..), editListLen, exZipTime, getParams, zipTimeToNum)
import Test.Assert (assert')

main :: Effect Unit
main = do
  log "getParams"
  assert' "5 6" (getParams 5 6  == 5:Nil)
  assert' "100 64" (getParams 100 64  == 36:1:Nil)

  log "editListLen"
  assert' "Nil 2" (editListLen Nil 2 10 == 0:10:Nil)

  log "zipTimeToNum"
  assert' "0 64" (zipTimeToNum 1 64 == 1:0:0:0:Nil)
  assert' "64 64" (zipTimeToNum 64 64 == 0:1:0:0:Nil)
  assert' "262144 64" (zipTimeToNum 262144 64 == 0:0:0:1:Nil)
  assert' "7864320 64" (zipTimeToNum 7864320 64 == 0:0:0:30:Nil)
  assert' "7864330 64" (zipTimeToNum 7864330 64 == 1:0:0:30:Nil)

  log "exZipTime"
  assert' "0" (exZipTime (Interval 0.0) == "0")