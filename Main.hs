{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Node (Node, Stmt(..), reduceWithSteps, reduceFullStmt, replace, reduceFull)
import NodeParser (parse, lambda)

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering, LineBuffering), hFlush)
import Control.Monad.State

import qualified Data.Map as M

lambdaPrelude :: [Stmt]
lambdaPrelude =
  [ [lambda| 0 = \f x. x |]
  , [lambda| 1 = succ 0 |]
  , [lambda| 2 = succ 1 |]
  , [lambda| 3 = succ 2 |]
  , [lambda| 4 = succ 3 |]
  , [lambda| 5 = succ 4 |]
  , [lambda| 6 = succ 5 |]
  , [lambda| 7 = succ 6 |]
  , [lambda| 8 = succ 7 |]
  , [lambda| 9 = succ 8 |]
  , [lambda| 10 = succ 9 |]
  , [lambda| succ = \n f x. f (n f x) |]
  , [lambda| add = \m n f x. m f (n f x) |]
  , [lambda| mul = \m n f. m (n f) |]
  , [lambda| power = \m n. n m |]
  , [lambda| fac = \n f. n (\f n. n (f (\f x. n f (f x)))) (\x. f) (\x. x) |]
  ]

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  evalStateT repl $ M.fromList $ map unwrapDecl lambdaPrelude

unwrapDecl :: Stmt -> (String, Node)
unwrapDecl (Decl name node) = (name, node)

type Vars = M.Map String Node

repl :: StateT Vars IO ()
repl = do
  lift $ putStr "> "
  lift $ hFlush stdout
  line <- lift getLine

  case parse line of
    Nothing -> lift (putStrLn "ERROR: parsing error")
    Just stmt ->
      case stmt of
        Decl name node -> modify (M.insert name node)
        Node node -> do
          -- TODO: this is very slow when there's a lot of globals
          globals_steps <- gets $ (node:) . flip applyGlobals node

          let all_steps = globals_steps ++ [reduceFull (last globals_steps)]
          let steps = init all_steps
          lift $ mapM_ (putStrLn . ("==> " ++) . show) steps
          let result = last all_steps
          lift $ putStrLn ("=> " ++ show result)
  repl

applyGlobals :: Vars -> Node -> [Node]
applyGlobals vars = map snd . takeWhile (uncurry (/=)) . (zip <$> init <*> tail) . iterate applyFromPairs
  where
    pairs = M.toList vars
    applyFromPairs node = foldl (flip $ uncurry replace) node pairs
