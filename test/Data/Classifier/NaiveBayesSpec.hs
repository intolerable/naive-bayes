module Data.Classifier.NaiveBayesSpec where

--import qualified Data.Classifier as Classifier
import Control.Applicative
import Control.Monad
import Data.Classifier (Classifier(..))
import Data.Classifier.NaiveBayes
import Data.Counter (Counter)
import Data.Map (Map)
import Data.Monoid
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

instance (Arbitrary a, Ord a, Arbitrary b, Ord b) => Arbitrary (NaiveBayes a b) where
  arbitrary = fromClassifier <$> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "NaiveBayes" $ do

    describe "Monoid" $ do
      prop "mempty <> x == x" $ \(x :: NaiveBayes String Int) ->
        mempty <> x == x
      prop "x <> mempty == x" $ \(x :: NaiveBayes String Int) ->
        x <> mempty == x
      prop "x <> (y <> z) == (x <> y) <> z" $ \(x :: NaiveBayes Int String) y z ->
        x <> (y <> z) == (x <> y) <> z

    describe "remove" $ do
      prop "remove x (x <> mempty) == mempty" $ \(x :: Classifier Int String) ->
        remove x (mappend (fromClassifier x) mempty) == mempty
      --prop "remove x (x <> y) == y" $ \(x :: Classifier Int String) y ->
      --  remove x (mappend (fromClassifier x) y) == y
