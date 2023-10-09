module Scale
  ( module Data.Pair
  , class Continuous
  , toVect
  , fromVect
  , linear
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.FastVect.Common as Common
import Data.FastVect.FastVect (Vect, (:))
import Data.FastVect.FastVect as FastVect
import Data.Number as Number
import Data.Pair (Pair, (~))
import Data.Reflectable (class Reflectable)
import Prim.Int (class Compare)
import Prim.Ordering (GT)

class Continuous a (n :: Int) where
  toVect :: a -> Vect n Number
  fromVect :: Vect n Number -> a

instance
  ( Compare n Common.Zero GT
  , Reflectable n Int
  ) =>
  Continuous Number n where
  toVect = FastVect.replicate (Common.term :: _ n)
  fromVect = FastVect.head

instance Continuous Color 3 where
  toVect color = do
    let { h, s, l } = Color.toHSLA color
    h : s : l : FastVect.empty
  fromVect vect = do
    let
      h = FastVect.index (Common.term :: _ 0) vect
      s = FastVect.index (Common.term :: _ 1) vect
      l = FastVect.index (Common.term :: _ 2) vect
    Color.hsl h s l

-- else instance
--   ( Continuous a 1
--   , Compare n Common.NegOne GT
--   , Compare m Common.NegOne GT
--   , Add 1 m n
--   , Reflectable n Int
--   ) =>
--   Continuous a n where
--   toVect x = do
--     let x' = FastVect.head (toVect x :: Vect 1 Number)
--     FastVect.replicate (Common.term :: _ n) x'
--   fromVect = fromVect <<< FastVect.take (Common.term :: _ 1)

linear
  :: forall domain range (@n :: Int)
   . Continuous domain n
  => Continuous range n
  => Compare n Common.NegOne GT
  => Reflectable n Int
  => { domain :: Pair domain
     , range :: Pair range
     }
  -> domain
  -> range
linear { domain, range } n = do
  let
    d1 ~ d2 = toVect @domain @n <$> domain
    r1 ~ r2 = toVect @range @n <$> range
    d = Number.abs <$> (d2 - d1)
    r = Number.abs <$> (r2 - r1)
    n' = toVect n
  fromVect ((/) <$> (r1 + (n' - d1) * r) <*> d)
