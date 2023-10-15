module Internal.Class.Linear
  ( class Interpolatable
  , interpolate
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Number as Number
import Data.Pair (Pair, (~))
import Data.Symbol as Symbol
import Prim.Row as Row
import Prim.RowList (class RowToList, Nil, RowList)
import Prim.RowList as RowList
import Record.Unsafe as Record.Unsafe
import Type.Prelude (class IsSymbol, Proxy(..))

class Interpolatable a where
  interpolate :: a -> a -> Number -> a

instance Interpolatable Number where
  interpolate n1 n2 d = n1 + (d * (n2 - n1))

instance Interpolatable Color where
  interpolate c1 c2 d = do
    let
      c = interpolate (Color.toRGBA' c1) (Color.toRGBA' c2) d
      r = clamp 0.0 1.0 c.r
      g = clamp 0.0 1.0 c.g
      b = clamp 0.0 1.0 c.b
      a = clamp 0.0 1.0 c.a
    Color.rgba' r g b a

-- | Like `%`, but always positive.
remainderPos :: Number -> Number -> Number
remainderPos x y = (x Number.% y + y) Number.% y

infixl 7 remainderPos as %

instance Interpolatable a => Interpolatable (Pair a) where
  interpolate p1 p2 d = do
    let
      x1 ~ y1 = p1
      x2 ~ y2 = p2
      x = interpolate x1 x2 d
      y = interpolate y1 y2 d
    x ~ y

instance (RowToList row list, InterpolatableRecord list row row) => Interpolatable (Record row) where
  interpolate = interpolateRecord (Proxy :: Proxy list)

class InterpolatableRecord :: RowList Type -> Row Type -> Row Type -> Constraint
class InterpolatableRecord rowlist row subrow | rowlist -> subrow where
  interpolateRecord :: Proxy rowlist -> Record row -> Record row -> Number -> Record subrow

instance InterpolatableRecord Nil row () where
  interpolateRecord _ _ _ _ = {}

instance
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , InterpolatableRecord rowlistTail row subrowTail
  , Interpolatable focus
  ) =>
  InterpolatableRecord (RowList.Cons key focus rowlistTail) row subrow where
  interpolateRecord _ ra rb d = insert (interpolate (get ra) (get rb) d) tail
    where
    insert = Record.Unsafe.unsafeSet key :: focus -> Record subrowTail -> Record subrow
    key = Symbol.reflectSymbol (Proxy :: Proxy key)
    get = Record.Unsafe.unsafeGet key :: Record row -> focus
    tail = interpolateRecord (Proxy :: Proxy rowlistTail) ra rb d
