{-# OPTIONS_GHC -Wno-missing-fields #-}

module NodeParser
  ( parse
  , parseM
  , lambda
  ) where

import Node (Node(..))
import Parser
import Control.Monad (mfilter, replicateM)
import Control.Applicative (asum, some)
import Data.Char (isAlphaNum)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (liftData)

parse :: String -> Maybe Node
parse = fmap fst . mfilter (null . snd) . runParserT (parseNode <* ws)

parseNode :: Parser String Node
parseNode = foldl1 App <$> some (ws *> parseSingleNode)

parseSingleNode :: Parser String Node
parseSingleNode = asum [parseClosed, parseAbs, parseParam]

parseClosed :: Parser String Node
parseClosed = eqP '(' *> ws *> parseNode <* ws <* eqP ')'

parseParamName :: Parser String String
parseParamName = mfilter (not . null) $ spanP isAlphaNum

parseParam :: Parser String Node
parseParam = Param <$> parseParamName

parseAbs :: Parser String Node
parseAbs = foldl1 (.) . map Abs <$> (eqP '\\' *> parseParams <* ws <* eqP '.') <*> parseNode
  where
    parseParams = some (ws *> parseParamName)


-- quasiquotes stuff

parseM :: MonadFail m => String -> m Node
parseM input =
  case parse input of
    Just value -> return value
    Nothing -> fail "parsing error"

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s = do parseM s >>= liftData

lambda :: QuasiQuoter
lambda = QuasiQuoter
  { quoteExp = quoteExprExp
  }
