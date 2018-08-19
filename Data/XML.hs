{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.XML where

import Control.Arrow
import Data.Fix
import Data.Functor.Classes
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Vector.Generic as V
import Data.Vector.Multi (Vector)
import Util

data ElementF a = ElementF
  { nameF :: !Text
  , attrsF :: !(Vector (Text, Text))
  , contentF :: !a }
  deriving (Functor, Foldable, Traversable)

instance Eq1 ElementF where
    liftEq eq x y = nameF x == nameF y && V.toList (attrsF x) == V.toList (attrsF y) && contentF x `eq` contentF y

instance Eq a => Eq (ElementF a) where (==) = eq1

data ContentF a = ContentF !L.Text !(Vector (NonEmpty a, L.Text))
instance Functor ContentF where fmap f (ContentF xs cs) = Content xs $ V.map (fmap f *** id) cs
instance Foldable ContentF where foldMap f (ContentF _ cs) = foldMap (foldMap f . fst) (V.toList cs)
instance Traversable ContentF where traverse f (ContentF xs cs) = Content xs . V.fromList <$> traverse (traverse f *=* pure) (V.toList cs)
instance Eq1 ContentF where
    liftEq eq (ContentF xs cs) (ContentF ys ds) = xs == ys && liftEq (liftEq2 (liftEq eq) (==)) (V.toList cs) (V.toList ds)

instance Eq a => Eq (ContentF a) where (==) = eq1

instance Semigroup (ContentF a) where
    ContentF xs cs <> ContentF ys ds = case V.unsnoc cs of
        Nothing -> ContentF (xs <> ys) ds
        Just (cs, (es, zs)) -> ContentF xs (cs V.++ V.cons (es, zs <> ys) ds)

instance Monoid (ContentF a) where mempty = ContentF mempty V.empty

type Element = Fix (Compose ElementF ContentF)
type Content = ContentF Element

pattern Element { name, attrs, content } = Fix (Compose (ElementF { nameF = name, attrsF = attrs, contentF = content }))
{-# COMPLETE Element #-}

pattern Content :: L.Text -> Vector (NonEmpty a, L.Text) -> ContentF a
pattern Content xs cs = ContentF xs cs
{-# COMPLETE Content #-}

showElement :: Element -> L.Text
showElement (Element {..}) =
    mconcat
    ["<",
     intercalate " " . fmap L.fromStrict $
     name :| ((\ (a, b) -> a <> "=\"" <> b <> "\"") <$> V.toList attrs),
     case content of
         Content "" (V.null -> True) -> "/>"
         Content xs cs -> ">" <> xs <>
                          foldMap (\ (es, xs) -> foldMap showElement es <> xs) (V.toList cs) <>
                          "</" <> L.fromStrict name <> ">"]
