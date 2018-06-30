module Main where

import Prelude

import Data.List (List(..), fromFoldable)
import Effect (Effect)
import Effect.Class.Console (log)

main :: forall e. Effect Unit
main = do
  log "Hello sailor!"

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true