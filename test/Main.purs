module Test.Main
  ( main
  ) where

import Prelude

import Color as Color
import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as Uncurried
import Debug as Debug
import Effect (Effect)
import Scale ((~))
import Scale as Scale

foreign import scaleLinear_
  :: forall range
   . Fn2 (Array Number) (Array range)
       (Number -> range)

main :: Effect Unit
main = do
  let
    scaleColor = Scale.linear @3 { domain: 0.0 ~ 100.0, range: Color.white ~ Color.black }
    scaleColor_ = Uncurried.runFn2 scaleLinear_ ([ 0.0, 100.0 ]) ([ "white", "black" ])

  Debug.traceM (scaleColor 10.0)
  Debug.traceM (scaleColor_ 10.0)

  Debug.traceM "\n"

  Debug.traceM (scaleColor 80.0)
  Debug.traceM (scaleColor_ 80.0)

  Debug.traceM "\n"

  Debug.traceM (scaleColor 60.0)
  Debug.traceM (scaleColor_ 60.0)

  -- Debug.traceM (scaleColor 64.0)
  -- Debug.traceM (scaleColor_ 64.0)

  -- Debug.traceM (scaleColor 0.0)
  -- Debug.traceM (scaleColor_ 0.0)

  -- Debug.traceM (scaleColor 100.0)
  -- Debug.traceM (scaleColor_ 100.0)

  pure unit
