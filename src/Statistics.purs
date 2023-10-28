module Statistics
  ( extent
  , extentBy
  , extentWith
  , variance
  , deviation
  , quantile
  , ticks
  , tickStep
  , range
  ) where

import Prelude

import Data.Int as Int
import Data.Int.Bits as Bits
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Pair (Pair, (~))

range
  :: Number
  -> Number
  -> Number
  -> List Number
range start stop step =
  let
    n =
      (stop - start)
        / step
        # Int.ceil
        -- get rid of NaN
        # Bits.or 0
        # max 0
        # Int.toNumber

    helper i list =
      if i >= 0.0 then
        helper (i - 1.0) ((start + step * i) : list)

      else
        list
  in
    helper (n - 1.0) Nil

ticks
  :: Number
  -> Number
  -> Int
  -> List Number
ticks start stop count =
  let
    step =
      tickStep start stop count

    beg =
      Int.toNumber (Int.ceil (start / step)) * step

    end =
      Int.toNumber (Int.floor (stop / step)) * step + step / 2.0
  in
    range beg end step

logBase
  :: Number
  -> Number
  -> Number
logBase b n = Number.log n / Number.log b

tickStep
  :: Number
  -> Number
  -> Int
  -> Number
tickStep start stop count =
  let
    step0 =
      Number.abs (stop - start) / max 0.0 (Int.toNumber count)

    step1 =
      Int.toNumber (10 `Int.pow` Int.floor (logBase Number.e step0 / logBase Number.e 10.0))

    error =
      step0 / step1

    step2 =
      if error >= Number.sqrt 50.0 then
        step1 * 10.0

      else if error >= Number.sqrt 10.0 then
        step1 * 5.0

      else if error >= Number.sqrt 2.0 then
        step1 * 2.0

      else
        step1
  in
    if stop < start then
      -step2

    else
      step2

extent
  :: forall a
   . Ord a
  => List a
  -> Maybe (Pair a)
extent =
  extentBy identity

extentBy
  :: forall a b
   . Ord b
  => (a -> b)
  -> List a
  -> Maybe (Pair a)
extentBy fn list =
  let
    min a b =
      if fn a < fn b then
        a

      else
        b

    max a b =
      if fn a > fn b then
        a

      else
        b

    helper l (mini ~ maxi) =
      case l of
        Nil ->
          mini ~ maxi

        x : xs ->
          helper xs (min mini x ~ max maxi x)
  in
    case list of
      Nil ->
        Nothing

      x : xs ->
        Just (helper xs (x ~ x))

extentWith
  :: forall a
   . (a -> a -> Ordering)
  -> List a
  -> Maybe (Pair a)
extentWith toOrder list =
  let
    max a b =
      case toOrder a b of
        GT ->
          a

        _ ->
          b

    folder (mini ~ maxi) element =
      case toOrder element mini of
        LT ->
          -- if new is less than mini, it can never be larger than maxi
          -- so we're immediately done
          element ~ maxi

        EQ ->
          -- idem
          element ~ maxi

        GT ->
          mini ~ (max element maxi)
  in
    case list of
      Nil ->
        Nothing

      x : xs ->
        Just (List.foldl folder (x ~ x) xs)

variance
  :: List Number
  -> Maybe Number
variance nums =
  let
    compute value { avg, i, sum } =
      let
        delta =
          value - avg

        newMean =
          avg + delta / (i + 1.0)
      in
        { avg: newMean, i: i + 1.0, sum: sum + delta * (value - newMean) }

    { i: length, sum } =
      List.foldr compute { avg: 0.0, i: 0.0, sum: 0.0 } nums
  in
    if length > 1.0 then
      Just (sum / (length - 1.0))

    else
      Nothing

deviation
  :: List Number
  -> Maybe Number
deviation =
  variance >>> map Number.sqrt

quantile
  :: Number
  -> List Number
  -> Maybe Number
quantile p values =
  if p <= 0.0 then
    List.head values

  else if p >= 1.0 then
    List.last values

  else
    case values of
      Nil ->
        Nothing

      x : y : _ ->
        let
          n =
            List.length values # Int.toNumber

          i =
            (n - 1.0) * p

          i0 =
            Int.floor i

          value0 =
            List.index values i0 # Maybe.fromMaybe x

          value1 =
            List.index values (i0 + 1) # Maybe.fromMaybe y
        in
          Just (value0 + (value1 - value0) * (i - Int.toNumber i0))

      head : _ ->
        Just head
