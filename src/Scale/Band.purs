module Scale.Band
  ( bandwidth
  , convert
  ) where

import Prelude

import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Pair (Pair, (~))

type Config =
  { paddingInner :: Number
  , paddingOuter :: Number
  , align :: Number
  }

normalizeConfig
  :: Config
  -> Config
normalizeConfig cfg =
  { paddingInner: clamp 0.0 1.0 cfg.paddingInner
  , paddingOuter: clamp 0.0 1.0 cfg.paddingOuter
  , align: clamp 0.0 1.0 cfg.align
  }

bandwidth
  :: forall a
   . Config
  -> List a
  -> Pair Number
  -> Number
bandwidth cfg domain (d0 ~ d1) =
  let
    { paddingInner, paddingOuter } =
      normalizeConfig cfg

    start ~ stop =
      if d0 < d1 then d0 ~ d1
      else d1 ~ d0

    n =
      Int.toNumber (List.length domain)

    step =
      (stop - start) / max 1.0 (n - paddingInner + paddingOuter * 2.0)
  in
    step * (1.0 - paddingInner)

computePositions
  :: Config
  -> Number
  -> Pair Number
  -> Pair Number
computePositions cfg n (start ~ stop) =
  let
    { paddingInner, paddingOuter, align } =
      normalizeConfig cfg

    step =
      (stop - start) / max 1.0 (n - paddingInner + paddingOuter * 2.0)

    start2 =
      start + (stop - start - step * (n - paddingInner)) * align
  in
    start2 ~ step

convert
  :: forall a
   . Eq a
  => Config
  -> List a
  -> Pair Number
  -> a
  -> Number
convert cfg domain (start ~ stop) value =
  case List.findIndex (_ == value) domain of
    Just index' ->
      let
        index = Int.toNumber index'
        n =
          Int.toNumber (List.length domain)
      in
        if start < stop then
          let
            start2 ~ step =
              computePositions cfg n (start ~ stop)
          in
            start2 + step * index

        else
          let
            stop2 ~ step =
              computePositions cfg n (stop ~ start)
          in
            stop2 + step * (n - index - 1.0)

    Nothing ->
      0.0 / 0.0
