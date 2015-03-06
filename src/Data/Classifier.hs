module Data.Classifier
  ( Classifier(..)
  , singleton
  , train
  , documentCount ) where

import Data.Counter (Counter)
import Data.Default
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map

data Classifier a b = Classifier { toMap :: Map a [Counter b] }
  deriving (Show, Read, Eq)

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