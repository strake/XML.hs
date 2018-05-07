{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Data.XML where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import qualified Data.Text.Lazy as L
import qualified Data.Vector.Generic as V
import Data.Vector.Multi (Vector)
import Util

data Element = Element
  { name :: !Text
  , attrs :: !(Vector (Text, Text))
  , content :: !Content }

data Content = Content !L.Text !(Vector ([Element], L.Text))

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
