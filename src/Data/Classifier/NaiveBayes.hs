module Data.Classifier.NaiveBayes
  ( test
  , probabilities
  , priors
  , conditionalProbability ) where

import Data.Classifier
import Data.Counter (Counter(Counter))
import Data.Map (Map)
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Counter as Counter
import qualified Data.Map.Strict as Map

test :: (Ord a, Ord b) => Classifier a b -> Counter b -> Maybe a
test cls cnt =
  case Map.toList $ probabilities cls cnt of
    [] -> Nothing
    (x, _):_ -> Just x

probabilities :: (Ord a, Ord b) => Classifier a b -> Counter b -> Map a Rational
probabilities cls =
  applyMultinomial (priors cls) (conditionalProbability cls)

conditionalProbability :: (Ord a, Ord b) => Classifier a b -> Map a (Map b Rational)
conditionalProbability (Classifier m) =
  fM (tct $ Classifier m) $ \cls cnt ->
    Map.insert cls
      (Map.mergeWithKey
        (\_ numInVocabForWord numInClassForWord -> Just $ numInClassForWord % numInVocabForWord)
        (fmap (1%))
        (const Map.empty)
        ws' cnt)
  where
    fM x f = Map.foldrWithKey f Map.empty x
    ws' = Counter.toMap $ mconcat $ mconcat $ Map.elems m

priors :: (Ord a, Ord b) => Classifier a b -> Map a Rational
priors c@(Classifier m) = fmap (\cs -> fromIntegral (length cs) % documentCount c) m

applyMultinomial :: (Ord a, Ord b) => Map a Rational -> Map a (Map b Rational) -> Counter b -> Map a Rational
applyMultinomial prior condprob (Counter.toMap -> m) = f
  where
    f = Map.intersectionWith (*) prior $ fmap product h
    h = fmap (\n -> Map.elems $ Map.intersectionWith (\x y -> x * fromIntegral y) n m) condprob

tct :: (Ord a, Ord b) => Classifier a b -> Map a (Map b Integer)
tct (Classifier m) = fmap (Counter.toMap . mconcat) m

-- def train(classes, data):
--   words = extract_vocab(data)
--   n = count_docs(data)
--   prior = {}
--   condprob = {}
--   for class in classes:
--     condprob[class] = {}
--     nc = count_docs_in_class(data, class)
--     prior[class] = nc / n
--     class_text = docs_in_class(data, class).concat_words()
--     for t in words:
--       token_count = count_tokens_of_term(class_text, t)
--       condprob[class][t] = (token_count + 1) / (sum [class_text[t2][c] for t2 in words])
--    return V, prior, condprob

-- TRAINMULTINOMIALNB(C, D)
-- 1 V ← EXTRACTVOCABULARY(D)
-- 2 N ← COUNTDOCS(D)
-- 3 for each c ∈ C
-- 4 do Nc ← COUNTDOCSINCLASS(D, c)
-- 5 prior[c] ← Nc/N
-- 6 textc ← CONCATENATETEXTOFALLDOCSINCLASS(D, c)
-- 7 for each t ∈ V
-- 8 do Tct ← COUNTTOKENSOFTERM(textc, t)
-- 9 for each t ∈ V
-- 10 do condprob[t][c] ← Tct+1
-- ∑t
-- ′(Tct′+1)
-- 11 return V, prior, condprob
