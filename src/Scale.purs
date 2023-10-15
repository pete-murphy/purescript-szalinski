module Scale
  ( linear
  , linearBy
  , module Data.Pair
  ) where

import Prelude

import Data.Pair (Pair, (~))
import Internal.Class.Linear as Linear

linear
  :: forall range
   . Linear.Interpolatable range
  => { domain :: Pair Number
     , range :: Pair range
     , clamp :: Boolean
     }
  -> Number
  -> range
linear =
  linearBy Linear.interpolate

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

