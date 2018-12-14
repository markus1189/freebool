module BoolAlg
    ( BoolAlg (..)
    , fromBool
    , and
    , or
    ) where

import           Control.Applicative (liftA2)
import           Data.Function (const)
import           Data.Functor (fmap)
import qualified Prelude
import Data.List (foldl')

class BoolAlg b where
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  xor :: b -> b -> b
  not :: b -> b
  true :: b
  false :: b

and :: BoolAlg b => Prelude.Foldable f => f b -> b
and = foldl' (&&) true

or :: BoolAlg b => Prelude.Foldable f => f b -> b
or = foldl' (||) false

infixr 3 &&
infixr 2 ||
infixr 1 `xor`

instance BoolAlg Prelude.Bool where
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  xor x y = x && not y || not x && y
  not = Prelude.not
  true = Prelude.True
  false = Prelude.False

instance BoolAlg b => BoolAlg (a -> b) where
  (&&) = liftA2 (&&)
  (||) = liftA2 (||)
  xor = liftA2 xor
  not = fmap not
  true = const true
  false = const false

fromBool :: BoolAlg b => Prelude.Bool -> b
fromBool Prelude.True = true
fromBool Prelude.False = false
