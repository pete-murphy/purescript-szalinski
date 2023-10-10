module Scale
  ( module Data.Pair
  , class Continuous
  , toVect
  , fromVect
  , linear
  , linear'
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.FastVect.Common as Common
import Data.FastVect.FastVect (Vect, (:))
import Data.FastVect.FastVect as FastVect
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

instance Continuous (Pair Number) 2 where
  toVect (n1 ~ n2) = n1 : n2 : FastVect.empty
  fromVect vect = do
    let
      n1 = FastVect.index (Common.term :: _ 0) vect
      n2 = FastVect.index (Common.term :: _ 1) vect
    n1 ~ n2

instance Continuous Color 3 where
  toVect color = do
    let
      { h, s, l } = Color.toHSLA color
    h : s : l : FastVect.empty
  fromVect vect = do
    let
      h' = FastVect.index (Common.term :: _ 0) vect
      h = if h' < 0.0 then h' + 360.0 else if h' > 360.0 then h' - 360.0 else h'
      s = FastVect.index (Common.term :: _ 1) vect `min` 1.0
      l = FastVect.index (Common.term :: _ 2) vect `min` 1.0
    Color.hsl h s l

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
linear { domain, range } d = do
  let
    d1 ~ d2 = toVect @domain @n <$> domain
    r1 ~ r2 = toVect @range @n <$> range
    d' = toVect d
    v = r1 + ((/) <$> ((d' - d1) * (r2 - r1)) <*> (d2 - d1))
  fromVect v

linear'
  :: { domain :: Pair Number
     , range :: Pair Number
     }
  -> Number
  -> Number
linear' { domain, range } d' = do
  let
    d1 ~ d2 = domain
    r1 ~ r2 = range
  --                    (d - d1) / (d2 - d1)   == (r - r1) / (r2 - r1)
  --       (r2 - r1) * ((d - d1) / (d2 - d1))  ==  r - r1
  -- r1 + ((r2 - r1) * ((d - d1) / (d2 - d1))) ==  r 
  r1 + (d' - d1) * (r2 - r1) / (d2 - d1)
