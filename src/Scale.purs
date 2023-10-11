module Scale
  ( class Continuous
  , interpolate
  , linear
  , linearBy
  , module Data.Pair
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Pair (Pair, (~))

class Continuous a where
  interpolate :: a -> a -> Number -> a

instance Continuous Number where
  interpolate n1 n2 d = n1 + (d * (n2 - n1))

-- TODO: Distinguish color spaces to interpolate within?
instance Continuous Color where
  interpolate c1 c2 d = do
    let
      hsla1 = Color.toHSLA c1
      hsla2 = Color.toHSLA c2
      h' = interpolate hsla1.h hsla2.h d
      h = if h' < 0.0 then h' + 360.0 else if h' > 360.0 then h' - 360.0 else h'
      s = clamp 0.0 1.0 (interpolate hsla1.s hsla2.s d)
      l = clamp 0.0 1.0 (interpolate hsla1.l hsla2.l d)
    Color.hsl h s l

instance Continuous a => Continuous (Pair a) where
  interpolate p1 p2 d = do
    let
      x1 ~ y1 = p1
      x2 ~ y2 = p2
      x = interpolate x1 x2 d
      y = interpolate y1 y2 d
    x ~ y

linear
  :: forall range
   . Continuous range
  => { domain :: Pair Number
     , range :: Pair range
     , clamp :: Boolean
     }
  -> Number
  -> range
linear =
  linearBy interpolate

type Interpolator range = range -> range -> Number -> range

linearBy
  :: forall range
   . Interpolator range
  -> { domain :: Pair Number
     , range :: Pair range
     , clamp :: Boolean
     }
  -> Number
  -> range
linearBy interpolator params d = do
  let
    d1 ~ d2 = params.domain
    r1 ~ r2 = params.range
    d' = if params.clamp then clamp d1 d2 d else d
  interpolator r1 r2 ((d' - d1) / (d2 - d1))