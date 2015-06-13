module Data.ClassifierSpec where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Classifier (Classifier(..))
import Data.Counter (Counter)
import Data.Map (Map)
import Data.Monoid
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Counter as Counter
import qualified Data.Map as Map

instance (Arbitrary a, Ord a) => Arbitrary (Counter a) where
  arbitrary = Counter.fromList <$> arbitrary
  shrink c = Counter.fromCounts <$> shrink (Counter.toList c)

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = Map.fromList <$> arbitrary
  shrink m = Map.fromList <$> shrink (Map.toList m)

instance (Arbitrary a, Ord a, Arbitrary b, Ord b) => Arbitrary (Classifier a b) where
  -- weird arbitrary definition or we get GIGANTIC Classifiers
  arbitrary = Classifier <$> Map.fromList <$> replicateM 5 ((,) <$> arbitrary <*> replicateM 5 (Counter.fromCounts <$> vector 5))
  shrink (Classifier m) = Classifier <$> shrink m

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Classifier" $ do

    describe "Binary" $ do
      prop "decode . encode == id" $ \(x :: Classifier String Int) ->
        decode (encode x) == x

    describe "Monoid" $ do
      prop "mempty <> x == x" $ \(x :: Classifier String Int) ->
        mempty <> x == x
      prop "x <> mempty == x" $ \(x :: Classifier String Int) ->
        x <> mempty == x
      prop "x <> (y <> z) == (x <> y) <> z" $ \(x :: Classifier Int String) y z ->
        x <> (y <> z) == (x <> y) <> z
