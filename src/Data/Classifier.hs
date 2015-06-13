module Data.Classifier
  ( Classifier(..)
  , singleton
  , train
  , documentCount
  , countInClass
  , totalInClass ) where

import Data.Binary
import Data.Counter (Counter)
import Data.Default
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Counter as Counter

newtype Classifier a b = Classifier { toMap :: Map a [Counter b] }
  deriving (Show, Read, Eq, Binary)

instance Ord a => Monoid (Classifier a b) where
  mempty = Classifier mempty
  Classifier m `mappend` Classifier n =
    Classifier (Map.unionWith (<>) m n)

instance Default (Classifier a b) where def = Classifier def

singleton :: a -> Counter b -> Classifier a b
singleton c v = Classifier $ Map.singleton c [v]

-- | @train c v x@ adds the key @(c, v)@ to the 'Classifier' @x@.
train :: (Ord a, Ord b) => a -> Counter b -> Classifier a b -> Classifier a b
train c v (Classifier m) = Classifier $ Map.insertWith (<>) c [v] m

documentCount :: Classifier a b -> Integer
documentCount (Classifier m) = fromIntegral $ length $ mconcat $ Map.elems m

countInClass :: Ord b => Classifier a b -> Map a (Counter b)
countInClass (Classifier m) = fmap mconcat m

totalInClass :: Ord b => Classifier a b -> Counter a
totalInClass = Counter.fromMap . fmap Counter.total . countInClass
