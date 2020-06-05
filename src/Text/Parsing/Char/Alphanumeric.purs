module Text.Parsing.Char.Alphanumeric
  ( lowercase
  , uppercase
  , any
  ) where

import Prelude

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

digits :: Array Char
digits = [ '0','1','2','3','4','5','6','7','8','9' ]

lowercases :: Array Char
lowercases = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

uppercases :: Array Char
uppercases = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]

-- | Consumes a valid digit character (0-9), or fails otherwise.
digit :: forall a m. S.StringLike a => Monad m => ParserT a m Char
digit = C.choice (S.char <$> digits)

-- | Consumes a valid uppercase alphanumeric character (A-Z|0-9), or fails otherwises.
lowercase :: forall a m. S.StringLike a => Monad m => ParserT a m Char
lowercase = C.choice (S.char <$> digits <> lowercases)

-- | Consumes a valid uppercase alphanumeric character (a-z|0-9), or fails otherwises.
uppercase :: forall a m. S.StringLike a => Monad m => ParserT a m Char
uppercase = C.choice (S.char <$> digits <> uppercases)

-- | Consumes a valid uppercase alphanumeric character (a-z|A-Z|0-9), or fails otherwises.
any :: forall a m. S.StringLike a => Monad m => ParserT a m Char
any = C.choice (S.char <$> digits <> lowercases <> uppercases)
