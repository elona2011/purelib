module Script (Options, addScript) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), fromRight)
import Data.List.Types (NonEmptyList(..))
import Effect (Effect)
import Effect.Exception (Error)
import Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, typeOf, unsafeFromForeign, unsafeReadTagged)
import Foreign.Index ((!))
import Partial.Unsafe (unsafePartial)

foreign import _addScript :: forall a. String
  -> (Error -> Either Error a)
  -> (a -> Either Error a)
  -> (Either Error a -> Effect Unit)
  -> Effect Unit

type Options a = {success :: a -> Effect Unit, fail :: Error -> Effect Unit}

addScript :: forall eff. String -> Foreign -> Effect Unit
addScript url params 
  | isNull params = _addScript url Left Right \r -> pure unit
  | isUndefined params = _addScript url Left Right \r -> pure unit
  | otherwise = _addScript url Left Right \r ->
    case r of 
      Left e -> unsafePartial fromRight $ runExcept $ params ! "fail" >>= readFunction >>= \cb -> pure $ cb e 
      Right e -> unsafePartial fromRight $ runExcept $ params ! "success" >>= readFunction >>= \cb -> pure $ cb e 

readFunction :: forall a. Foreign -> F (a -> Effect Unit)
readFunction value
  | typeOf value == "function" = pure (unsafeFromForeign value)
  | otherwise = fail $ TypeMismatch "functions" (typeOf value) 

-- runCb :: forall a. (a -> Effect Unit) ->