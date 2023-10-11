module Test.Main
  ( main
  ) where

import Prelude

import Color as Color
import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as Uncurried
import Effect (Effect)
import Effect.Aff as Aff
import Scale ((~))
import Scale as Scale
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner

foreign import scaleLinear_
  :: forall range
   . Fn2 (Array Number) (Array range)
       (Number -> range)

main :: Effect Unit
main = Aff.launchAff_ do
  Runner.runSpec [ Reporter.consoleReporter ] do
    Spec.describe "linear" do

      Spec.it "should interpolate range of numbers" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: 0.0 ~ 20.0
              , clamp: false
              }
        scale 0.0 `shouldEqual` 0.0
        scale 25.0 `shouldEqual` 5.0

      Spec.it "should extrapolate range of numbers" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: 0.0 ~ 20.0
              , clamp: false
              }
        scale 200.0 `shouldEqual` 40.0

      Spec.it "should interpolate range of pairs of numbers" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: (0.0 ~ 80.0) ~ (10.0 ~ 100.0)
              , clamp: false
              }
        scale 0.0 `shouldEqual` (0.0 ~ 80.0)
        scale 25.0 `shouldEqual` (2.5 ~ 85.0)

      Spec.it "should interpolate hue in color range" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: Color.hsl 100.0 0.5 0.5 ~ Color.hsl 200.0 0.5 0.5
              , clamp: false
              }
        scale 50.0 `shouldEqual` Color.hsl 150.0 0.5 0.5
        scale (-50.0) `shouldEqual` Color.hsl 50.0 0.5 0.5

      Spec.it "should interpolate hue, saturation, and luminance in color range" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: Color.hsl 100.0 0.0 0.0 ~ Color.hsl 200.0 1.0 1.0
              , clamp: false
              }
        scale 50.0 `shouldEqual` Color.hsl 150.0 0.5 0.5

      Spec.it "should extrapolate hue in color range" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: Color.hsl 100.0 0.5 0.5 ~ Color.hsl 200.0 0.5 0.5
              , clamp: false
              }
        scale (-50.0) `shouldEqual` Color.hsl 50.0 0.5 0.5
        scale (-200.0) `shouldEqual` Color.hsl 260.0 0.5 0.5
        scale 300.0 `shouldEqual` Color.hsl 40.0 0.5 0.5

      Spec.it "should match output of d3.scaleLinear" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: Color.hsl 100.0 0.5 0.5 ~ Color.hsl 200.0 0.5 0.5
              , clamp: false
              }
          scale' =
            Uncurried.runFn2 scaleLinear_
              [ 0.0, 100.0 ]
              [ Color.hsl 100.0 0.5 0.5, Color.hsl 200.0 0.5 0.5 ]
        scale 0.0 `shouldEqual` scale' 0.0
        scale 10.0 `shouldEqual` scale' 10.0
        scale 50.0 `shouldEqual` scale' 50.0
        scale 100.0 `shouldEqual` scale' 100.0
        scale 200.0 `shouldEqual` scale' 200.0
