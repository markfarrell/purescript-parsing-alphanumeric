module Test.Text.Parsing.String.Alphanumeric
  ( main
  ) where

import Prelude

import Effect (Effect)

import Text.Parsing.String.Alphanumeric as A

import Text.Parsing.Expect as E

digits :: String
digits = "0123456789"

lowercases :: String
lowercases = "abcdefghijklmnopqrstuvwxyz"

uppercases :: String
uppercases = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

any :: Effect Unit
any = do
  _ <- E.failure ""                    (A.any)
  _ <- E.failure "$"                   (A.any)
  _ <- E.failure "$$"                  (A.any)
  _ <- E.success "a"                   (A.any)
  _ <- E.success "A"                   (A.any)
  _ <- E.success "0"                   (A.any)
  _ <- E.success "1"                   (A.any)
  _ <- E.success "2"                   (A.any)
  _ <- E.success "3"                   (A.any)
  _ <- E.success "4"                   (A.any)
  _ <- E.success "5"                   (A.any)
  _ <- E.success "6"                   (A.any)
  _ <- E.success "7"                   (A.any)
  _ <- E.success "8"                   (A.any)
  _ <- E.success "9"                   (A.any)
  _ <- E.success digits                (A.any)
  _ <- E.success lowercases            (A.any)
  _ <- E.success uppercases            (A.any)
  _ <- E.success "012abc345ABC678efg9" (A.any)
  pure unit

lowercase :: Effect Unit
lowercase = do
  _ <- E.failure ""                   (A.lowercase)
  _ <- E.failure "$"                  (A.lowercase)
  _ <- E.failure "$$"                 (A.lowercase)
  _ <- E.success "a"                  (A.lowercase)
  _ <- E.failure "A"                  (A.lowercase)
  _ <- E.success "0"                  (A.lowercase)
  _ <- E.success digits               (A.lowercase)
  _ <- E.success lowercases           (A.lowercase)
  _ <- E.failure uppercases           (A.lowercase)
  _ <- E.output lowercases lowercases (A.lowercase)
  pure unit

uppercase :: Effect Unit
uppercase = do
  _ <- E.failure ""                   (A.uppercase)
  _ <- E.failure "$"                  (A.uppercase)
  _ <- E.failure "$$"                 (A.uppercase)
  _ <- E.failure "a"                  (A.uppercase)
  _ <- E.success "A"                  (A.uppercase)
  _ <- E.success "0"                  (A.uppercase)
  _ <- E.success digits               (A.uppercase)
  _ <- E.failure lowercases           (A.uppercase)
  _ <- E.success uppercases           (A.uppercase)
  _ <- E.output uppercases uppercases (A.uppercase)
  pure unit


main :: Effect Unit
main = do
  _ <- any
  _ <- lowercase
  _ <- uppercase
  pure unit
