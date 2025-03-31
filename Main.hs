module Main (main) where

import Node (Node)

import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))
import Control.Monad.State

import qualified Data.Map as M

type Vars = M.Map String Node

main :: IO ()
main = evalStateT repl mempty

repl :: StateT Vars IO ()
repl = do
  lift $ hSetBuffering stdin NoBuffering
  lift $ putStr "> "
  line <- lift getLine

  undefined -- parsing of statements

  repl
