module Node
  ( Stmt(..)
  , Node(..)
  , reduceOnce
  , reduceWithSteps
  , reduceFull
  , reduceFullStmt
  , replace
  ) where

import Data.Data (Data, Typeable)

data Stmt = Node Node | Decl String Node
  deriving (Eq, Data, Typeable)

instance Show Stmt where
  show (Node node) = show node
  show (Decl name node) = unwords [name, "=", show node]

data Node
  = Param String
  | Abs   String Node
  | App   Node   Node
  deriving (Eq, Data, Typeable)

isParam :: Node -> Bool
isParam (Param _) = True
isParam _ = False

isAbs :: Node -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

instance Show Node where
  show (Param name) = name
  show (Abs param body) = "\\" ++ param ++ ". " ++ show body
  show (App func arg) = unwords
    [ if isAbs func then "(" ++ show func ++ ")" else show func
    , if isParam arg then show arg else "(" ++ show arg ++ ")"
    ]

reduceOnce :: Node -> Node
reduceOnce (Param name) = Param name
reduceOnce (Abs param body) = Abs param (reduceOnce body)
reduceOnce (App (Abs param body) arg) = replace param arg body
reduceOnce (App func arg) = App (reduceOnce func) (reduceOnce arg)

reduceWithSteps :: Node -> [Node]
reduceWithSteps = map snd . takeWhile (uncurry (/=)) . (zip <$> init <*> tail) . iterate reduceOnce

reduceFull :: Node -> Node
reduceFull node = last (node : reduceWithSteps node)

reduceFullStmt :: Stmt -> Stmt
reduceFullStmt (Node node) = Node $ reduceFull node
reduceFullStmt (Decl name node) = Decl name $ reduceFull node

replace :: String -> Node -> Node -> Node
replace from to (Param name)
  | name == from = to
  | otherwise = Param name
replace from to (Abs param body)
  | param == from = Abs param body
  | otherwise = Abs param (replace from to body)
replace from to (App func arg) = App (replace from to func) (replace from to arg)
