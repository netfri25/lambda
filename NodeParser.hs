{-# OPTIONS_GHC -Wno-missing-fields #-}

module NodeParser
  ( parse
  , parseM
  , lambda
  ) where

import Node (Node(..), Stmt(..))
import Parser
import Control.Monad (mfilter, replicateM)
import Control.Applicative (asum, some)
import Data.Char (isAlphaNum)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (liftData)

parse :: String -> Maybe Stmt
parse = fmap fst . mfilter (null . snd) . runParserT (ws *> parseStmt <* ws)

parseStmt :: Parser String Stmt
parseStmt = asum [parseDecl, Node <$> parseNode]

parseDecl :: Parser String Stmt
parseDecl = Decl <$> parseName <* ws <* eqP '=' <* ws <*> parseNode

parseNode :: Parser String Node
parseNode = foldl1 App <$> some (ws *> parseSingleNode)

parseSingleNode :: Parser String Node
parseSingleNode = asum [parseClosed, parseAbs, parseParam]

parseClosed :: Parser String Node
parseClosed = eqP '(' *> ws *> parseNode <* ws <* eqP ')'

parseName :: Parser String String
parseName = mfilter (not . null) $ spanP isAlphaNum

parseParam :: Parser String Node
parseParam = Param <$> parseName

parseAbs :: Parser String Node
parseAbs = foldl1 (.) . map Abs <$> (eqP '\\' *> parseParams <* ws <* eqP '.') <*> parseNode
  where
    parseParams = some (ws *> parseName)


-- quasiquotes stuff

parseM :: MonadFail m => String -> m Stmt
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
