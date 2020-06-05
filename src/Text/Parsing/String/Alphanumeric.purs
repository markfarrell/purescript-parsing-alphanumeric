module Text.Parsing.String.Alphanumeric 
  ( lowercase
  , uppercase
  , any
  ) where

import Prelude

import Data.Foldable (foldMap)

import Data.String.CodeUnits (singleton)

import Data.Array as Array
import Data.List  as List

import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.String as S

import Text.Parsing.Char.Alphanumeric as A

-- | Consumes a non-empty string of lowercase alphanumeric characters, or fails otherwise.
lowercase :: forall a m. S.StringLike a => Monad m => ParserT a m String
lowercase = do
  x <- Array.fromFoldable <$> List.many A.lowercase
  case Array.length x > 0 of
    false -> fail $ "Number of lowercase alphanumeric characters must be positive." 
    true  -> pure $ foldMap singleton x 

-- | Consumes a non-empty string of uppercase alphanumeric characters, or fails otherwise.
uppercase :: forall a m. S.StringLike a => Monad m => ParserT a m String
uppercase = do
  x <- Array.fromFoldable <$> List.many A.uppercase
  case Array.length x > 0 of
    false -> fail $ "Number of uppercase alphanumeric characters must be positive." 
    true  -> pure $ foldMap singleton x 

-- | Consumes a non-empty string of alphanumeric characters, or fails otherwise.
any :: forall a m. S.StringLike a => Monad m => ParserT a m String
any = do
  x <- Array.fromFoldable <$> List.many A.any
  case Array.length x > 0 of
    false -> fail $ "Number of alphanumeric characters must be positive." 
    true  -> pure $ foldMap singleton x 
