module Data.Classifier.NaiveBayes
  ( test
  , probabilities
  , priors
  , conditionalProbabilities
  , module Data.Classifier ) where

import Data.Classifier
import Data.Counter (Counter(Counter))
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Ord
import Data.Ratio ((%))
import qualified Data.Counter as Counter
import qualified Data.Map.Strict as Map

test :: (Ord a, Ord b, Show b) => Classifier a b -> Counter b -> Maybe a
test cls cnt =
  case sortBy (comparing (Down . snd)) $ Map.toList $ probabilities cls cnt of
    [] -> Nothing
    (x, _):_ -> Just x

probabilities :: (Ord a, Ord b) => Classifier a b -> Counter b -> Map a Rational
probabilities cls =
  applyMultinomial (priors cls) (conditionalProbabilities cls)

conditionalProbabilities :: (Ord a, Ord b) => Classifier a b -> (Map b Integer, Map a Integer, Map a (Map b Rational))
conditionalProbabilities (Classifier m) =
  (combined, totalWordsInClass, Map.intersectionWith (\b t -> fmap (\x -> (x + 1) % b) t) divisor wordCountInClass)
  where totalWordsInClass = fmap (Counter.total . mconcat) m
        wordCountInClass = fmap (Counter.toMap . mconcat) m
        combined = Counter.toMap $ Map.foldr (<>) mempty $ fmap mconcat m
        totalCombined = Map.foldr (const (+1)) 0 combined
        divisor = fmap (+ totalCombined) totalWordsInClass

priors :: (Ord a, Ord b) => Classifier a b -> Map a Rational
priors c@(Classifier m) = fmap (\cs -> fromIntegral (length cs) % documentCount c) m

applyMultinomial :: (Ord a, Ord b) => Map a Rational -> (Map b Integer, Map a Integer, Map a (Map b Rational)) -> Counter b -> Map a Rational
applyMultinomial prior (global, classCount, condprobs) (Counter.toMap -> m) = f
  where
    f = Map.intersectionWith (*) prior $ fmap (Map.foldr (*) 1) qwer
    qwer = Map.mergeWithKey (\_ condprob r -> Just $ Map.mergeWithKey (\_ p t -> Just $ p ^ t ) (const mempty) (fmap (\w -> (1 % (r + total)) ^ w)) condprob m) (const mempty) (const mempty) condprobs classCount
    total = Map.foldr (const (+1)) (0 :: Integer) global
