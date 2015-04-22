module Data.Classifier.NaiveBayes
  ( NaiveBayes
  , fromClassifier
  , remove
  , test
  , probabilities ) where

import Data.Classifier
import Data.Counter (Counter(..))
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Ord
import Data.Ratio ((%))
import qualified Data.Counter as Counter
import qualified Data.Map.Strict as Map

data NaiveBayes a b =
  NaiveBayes { _vocab :: Counter b
             , _classInstances :: Counter a
             , _totalWordsInClass :: Counter a
             , _wordCounts :: Map a (Counter b) }
  deriving (Show, Read, Eq)

instance (Ord a, Ord b) => Monoid (NaiveBayes a b) where
  mempty = NaiveBayes mempty mempty mempty mempty
  NaiveBayes v1 ci1 t1 wc1 `mappend` NaiveBayes v2 ci2 t2 wc2 =
    NaiveBayes (v1 <> v2) (ci1 <> ci2) (t1 <> t2) (Map.unionWith (<>) wc1 wc2)

fromClassifier :: (Ord a, Ord b) => Classifier a b -> NaiveBayes a b
fromClassifier (Classifier m) = NaiveBayes v is t cs
  where v = Map.foldr (mappend . mconcat) mempty m
        is = Counter.fromMap $ fmap (fromIntegral . length) m
        t = Counter.fromMap $ fmap Counter.total cs
        cs = fmap mconcat m

remove :: (Ord a, Ord b) => Classifier a b -> NaiveBayes a b -> NaiveBayes a b
remove (Classifier m) nb = mappend nb $ fromClassifier $ Classifier $ fmap (fmap (Counter.unsafeFromMap . fmap negate . Counter.toMap)) m

probabilities :: (Ord a, Ord b) => NaiveBayes a b -> Counter b -> Map a Rational
probabilities (NaiveBayes (Counter.toMap -> v) (Counter.toMap -> c) t w) (Counter.toMap -> m) =
  Map.intersectionWith (*) priors' $ fmap (Map.foldr (*) 1) rationals
  where totalUniqueWords = Map.foldr (+) 0 $ fmap (const 1) v
        totalInstances = Counter.total $ Counter.fromMap c
        priors' = fmap (% totalInstances) c
        rationals = Map.intersectionWith (\ l (Counter.toMap -> r) ->
          Map.mergeWithKey (\ _ l' r' -> Just $ ((l' + 1) % l) ^ r') (const mempty) (fmap ((1 % l)^)) r m) divisors w
        divisors = fmap (+ totalUniqueWords) (Counter.toMap t)

test :: (Ord a, Ord b, Show b) => NaiveBayes a b -> Counter b -> Maybe a
test cls cnt =
  case sortBy (comparing (Down . snd)) $ Map.toList $ probabilities cls cnt of
    [] -> Nothing
    (x, _):_ -> Just x
