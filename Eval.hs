module Eval where
import Parser
import Data.Fixed
--import Debug.Trace
--data Token =
--    IntTok Int      |
--    RealTok Float   |
--    MultTok         |
--    AddTok          |
--    ExpTok          |
--    SubTok          |
--    DivTok          |
--    IntDivTok       |
--    LifeTok         |
--    PiTok           |
--    FogarteTok      |
--    TauTok          |
--    RoundTok        |
--    ModTok          |
--    LParenTok       |
--    RParenTok       |
--    IfzTok          |
--    ThenTok         |
--    ElseTok         |
--    MSTok           |
--    MRTok           |
--    DoneTok
--    deriving (Eq,Show)

--data Value = IntVal Int | RealVal Float deriving (Show, Eq)

--data Expr = 
--  MultExpr Expr Expr              | 
--  AddExpr Expr Expr               | 
--  ExpExpr Expr Expr               |  
--  SubExpr Expr Expr               | 
--  DivExpr Expr Expr               |
--  IntDivExpr Expr Expr            |
--  RoundExpr Expr                  |
--  ModExpr Expr Expr               |
--  IfzThenElseExpr Expr Expr Expr  |
--  TauExpr                         |
--  PiExpr                          |
--  FogarteExpr                     |
--  MRExpr                          |
--  NumExpr Value                   |
--  DoneExpr Expr                   |
--  NegExpr Expr
--  deriving (Show, Eq)
--
--data Stmt = 
--  Stmt Expr     |
--  MSStmt Expr   
--  deriving (Show, Eq)

negateV :: Value -> Value
negateV (IntVal v) = (IntVal (negate v))
negateV (RealVal v) = (RealVal (negate v))

roundV :: Value -> Value
roundV (IntVal v) = (IntVal v)
roundV (RealVal v) = (IntVal (round v))

arithOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> Value
arithOp f1 f2 (IntVal x) (IntVal y) = (IntVal ((f1) x y))
arithOp f1 f2 (RealVal x) (RealVal y) = (RealVal ((f2) x y))
arithOp f1 f2 (IntVal x) (RealVal y) = (RealVal ((f2) (fromIntegral x) y))
arithOp f1 f2 (RealVal x) (IntVal y) = (RealVal ((f2) x (fromIntegral y)))

multV :: Value -> Value -> Value
multV x y = (arithOp (*) (*) (x) (y))

addV :: Value -> Value -> Value
addV x y = (arithOp (+) (+) (x) (y))

powV :: Value -> Value -> Value
powV x y = (arithOp (^) (**) (x) (y))

subV :: Value -> Value -> Value
subV x y = (arithOp (-) (-) (x) (y))

modV :: Value -> Value -> Value
modV x y = (arithOp (mod) (mod') (x) (y))

divV :: Value -> Value -> Value
divV x y = (arithOp (div) (/) (x) (y))

intDivV :: Value -> Value -> Value
intDivV (IntVal x) (IntVal y) = (IntVal (x `div` y))
intDivV (IntVal x) (RealVal y) =
  let 
    (IntVal rY) = (roundV (RealVal y))
  in
    (IntVal (x `div` rY))
intDivV (RealVal x) (IntVal y) =
  let 
    (IntVal rX) = (roundV (RealVal x))
  in
    (IntVal (rX `div` y))
intDivV (RealVal x) (RealVal y) =
  let 
    (IntVal rX) = (roundV (RealVal x))
    (IntVal rY) = (roundV (RealVal y))
  in
    (IntVal (rX `div` rY))

isZero :: Value -> Bool
isZero (IntVal x) = if (x == 0) then True else False
isZero (RealVal x) = if (x == 0.0) then True else False

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

fogarteHelper :: (Fractional a, Show a, Eq a, Enum a) => a -> a
fogarteHelper 0 = 2
fogarteHelper n = 
  let
    x = (((2*n) + 2) / (factorial ((2*n) + 1)))
    next = (fogarteHelper (n-1)) 
  in
    (x + next)

eval :: Stmt -> IO Value
eval (Stmt e) = do
  res1 <- (evalExpr e)
  return (res1)
eval (MSStmt e) = do
  res1 <- (evalExpr e)
  return (res1)

evalExpr :: Expr -> IO Value
evalExpr (MultExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (multV (res1) (res2)) 
evalExpr (AddExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (addV (res1) (res2))
evalExpr (ExpExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (powV (res1) (res2)) 
evalExpr (SubExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (subV (res1) (res2))
evalExpr (DivExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (divV (res1) (res2))
evalExpr (IntDivExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (intDivV (res1) (res2))
evalExpr (RoundExpr e) = do
  res1 <- (evalExpr e)
  return (roundV (res1))
evalExpr (ModExpr e1 e2) = do
  res1 <- (evalExpr e1)
  res2 <- (evalExpr e2)
  return (modV (res1) (res2))
evalExpr (IfzThenElseExpr e1 e2 e3) = do
  res <- (evalExpr e1)
  case (isZero res) of
    True -> do
      res2 <- (evalExpr e2) 
      (return (res2))
    False -> do 
      res3 <- (evalExpr e3)
      (return (res3))
evalExpr TauExpr = do
  return (RealVal (realToFrac (2.0*pi)))
evalExpr PiExpr = do
  return (RealVal (realToFrac (pi)))
evalExpr FogarteExpr = do
  return (RealVal (fogarteHelper 6))
evalExpr MRExpr = do
  return (IntVal 0)
evalExpr (NegExpr e) = do
  res1 <- (evalExpr e)
  return (negateV (res1))
evalExpr (NumExpr v) = do
  return v
evalExpr (DoneExpr e) = do
  res1 <- (evalExpr e)
  return (res1)
