{-# OPTIONS_GHC -Wall -Wextra #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parser
  ( Parser
  , Input(..)
  , nextP
  , ifP
  , eqP
  , listP
  , spanP
  , sepBy
  , ws
  , runParserT
  ) where

import Control.Monad.State
import Data.List (uncons)
import Data.Char (isSpace)
import Control.Applicative (Alternative(..))

class Input i c | i -> c where
  inputNext :: i -> Maybe (c, i)

instance Input [a] a where
  inputNext = uncons

type Parser i a = StateT i Maybe a

runParserT :: Parser i a -> i -> Maybe (a, i)
runParserT = runStateT

nextP :: Input i c => Parser i c
nextP = StateT inputNext

ifP :: Input i c => (c -> Bool) -> Parser i c
ifP = flip mfilter nextP

eqP :: (Input i c, Eq c) => c -> Parser i c
eqP c = ifP (==c)

listP :: (Input i c, Eq c, Traversable t) => t c -> Parser i (t c)
listP = traverse eqP

spanP :: Input i c => (c -> Bool) -> Parser i [c]
spanP = many . ifP

sepBy :: Parser i a -> Parser i b -> Parser i [b]
sepBy sep parser = (:) <$> parser <*> many (sep *> parser) <|> pure []

ws :: Input i Char => Parser i String
ws = spanP isSpace
