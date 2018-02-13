module Eval where
import Parser
import Data.Fixed
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
--  MSExpr Expr                     |
--  MRExpr                          |
--  NumExpr Value                   |
--  DoneExpr Expr                   |
--  NegExpr Expr
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

fogarteHelper :: (Fractional a, Eq a, Enum a) => a -> a
fogarteHelper 0 = 0
fogarteHelper n = 
  let
    x = (((2*n) + 2) / (factorial ((2*n) + 1)))
    next = (fogarteHelper (n-1)) 
  in
    (x + next)

eval :: Expr -> Value
eval (MultExpr e1 e2) = (multV (eval e1) (eval e2))
eval (AddExpr e1 e2) = (addV (eval e1) (eval e2))
eval (ExpExpr e1 e2) =  (powV (eval e1) (eval e2))
eval (SubExpr e1 e2) = (subV (eval e1) (eval e2))
eval (DivExpr e1 e2) = (divV (eval e1) (eval e2))
eval (IntDivExpr e1 e2) = (intDivV (eval e1) (eval e2))
eval (RoundExpr e) = (roundV (eval e))
eval (ModExpr e1 e2) = (modV (eval e1) (eval e2))
eval (IfzThenElseExpr e1 e2 e3) = 
  let 
    res = (eval e1)
  in
    if (isZero res) then (eval e2) else (eval e3)
eval TauExpr = (RealVal (realToFrac (2.0*pi)))
eval PiExpr = (RealVal (realToFrac (pi)))
eval FogarteExpr = (RealVal (fogarteHelper 6))
eval (MSExpr e) = (eval e)
eval MRExpr = (IntVal 0)
eval (NegExpr e) = (negateV (eval e))
eval (NumExpr v) = v
eval (DoneExpr e) = (eval e)
