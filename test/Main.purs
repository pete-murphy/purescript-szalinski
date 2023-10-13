module Test.Main
  ( main
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Either as Either
import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as Uncurried
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Effect.Class
import Partial.Unsafe as Unsafe
import Scale ((~))
import Scale as Scale
import StringParser (Parser)
import StringParser as StringParser
import StringParser as StringParser.CodeUnits
import Test.QuickCheck ((===))
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Gen as Gen
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter as Reporter
import Test.Spec.Runner as Runner

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

      Spec.it "should interpolate red in color range" do
        let
          scale =
            Scale.linear
              { domain: 0.0 ~ 100.0
              , range: Color.rgb' 1.0 0.0 0.0 ~ Color.rgb' 0.0 1.0 0.5
              , clamp: false
              }
        scale 50.0 `shouldEqual` Color.rgb' 0.5 0.5 0.25

      Spec.it "should match output of d3.scaleLinear for color range" do
        Effect.Class.liftEffect do
          QuickCheck.quickCheckGen' 10_000 do
            d1 <- Gen.choose (-1000.0) 1000.0
            d2 <- Gen.choose (-1000.0) 1000.0
            d <- Gen.choose (-1000.0) 1000.0
            r1 <- Gen.uniform
            g1 <- Gen.uniform
            b1 <- Gen.uniform
            r2 <- Gen.uniform
            g2 <- Gen.uniform
            b2 <- Gen.uniform

            let
              rgb1 = Color.rgb' r1 g1 b1
              rgb2 = Color.rgb' r2 g2 b2
              scale =
                Scale.linear
                  { domain: d1 ~ d2
                  , range: rgb1 ~ rgb2
                  , clamp: false
                  }
              scale' =
                Uncurried.runFn2 scaleLinear_
                  [ d1, d2 ]
                  [ Color.toHexString rgb1, Color.toHexString rgb2 ]
              actual = Color.toHSLA (scale d)
              expected = Color.toHSLA (parseRGBString (scale' d))

            pure (actual === expected)

-- | `d3.scaleLinear`
foreign import scaleLinear_
  :: forall range
   . Fn2 (Array Number) (Array range)
       (Number -> range)

-- | `d3.scaleLinear` outputs an RGB string like `"rgb(255, 0, 0)"` when using
-- | its default color interpolator.
parseRGBString :: String -> Color
parseRGBString string =
  string # StringParser.runParser maybeColor
    # Either.hush
    # join
    # case _ of
        Just color -> color
        Nothing -> Unsafe.unsafeCrashWith ("parseRGBString failed on input: " <> string)

  where
  maybeInt :: Parser (Maybe Int)
  maybeInt = Int.fromString <$> StringParser.CodeUnits.regex "\\d+"

  maybeColor :: Parser (Maybe Color)
  maybeColor = do
    _ <- StringParser.string "rgb("
    r <- maybeInt
    _ <- StringParser.string ", "
    g <- maybeInt
    _ <- StringParser.string ", "
    b <- maybeInt
    pure (Color.rgb <$> r <*> g <*> b)
