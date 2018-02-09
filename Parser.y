{
module Eval where
import Lexer
import Data.Fixed.mod'
--data Token =
--    IntTok Int      |
--    RealTok Double  |
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
}

%monad { Maybe } 
%name parse
%tokentype {Token }
%error { parseError }

%token
    int     { IntTok $$ }
    real    { RealTok $$ }
    '*'     { MultTok }
    '+'     { AddTok } 
    '^'     { ExpTok }
    '-'     { SubTok }
    '/'     { DivTok }
    '//'    { IntDivTok }
    life    { LifeTok }
    pi      { PiTok }
    fogarte { FogarteTok }
    tau     { TauTok }
    '~'     { RoundTok }
    '%'     { ModTok }
    '('     { LParenTok }
    ')'     { RParenTok }
    ifz     { IfzTok }
    then    { ThenTok }
    else    { ElseTok }
    ms      { MSTok }
    mr      { MRTok }
    '?'     { DoneTok }

%left '+' '-'
%left '^' '*' '/' '//' '%'
%right ifz then else ms
%nonassoc NEG
%%

E : E '+' E                     { AddExpr $1 $3 }
  | E '^' E                     { ExpExpr $1 $3 }
  | E '*' E                     { MultExpr $1 $3 }
  | E '/' E                     { DivExpr $1 $3 }
  | E '//' E                    { IntDivExpr $1 $3 }
  | E '-' E                     { SubExpr $1 $3 }
  | '-' E %prec NEG             { NegExpr $2 }
  | '(' E ')'                   { $2 }
  | '~' E %prec NEG             { RoundExpr $2 }
  | E '%' E                     { ModExpr $1 $3 }
  | ifz E then E else E         { IfzThenElseExpr $2 $3 $4 }
  | E ms                        { MSExpr $1 }
  | mr                          { MRExpr }
  | tau                         { TauExpr }
  | pi                          { PiExpr }
  | life                        { IntVal 42 }
  | fogarte                     { FogarteExpr }
  | int                         { IntVal $1 }
  | real                        { RealVal $1 }

{

data Expr = 
  MultExpr Expr Expr              | 
  AddExpr Expr Expr               | 
  ExpExpr Expr Expr               |  
  SubExpr Expr Expr               | 
  DivExpr Expr Expr               |
  IntDivExpr Expr Expr            |
  RoundExpr Expr                  |
  ModExpr Expr Expr               |
  IfzThenElseExpr Expr Expr Expr  |
  TauExpr                         |
  PiExpr                          |
  FogarteExpr                     |
  MSExpr Expr                     |
  MRExpr Expr                     |
  NegExpr Expr
  deriving (Show, Eq)

data Value = IntVal Int | RealVal Float deriving (Show, Eq)

negateV :: Value -> Value
negateV (IntVal v) = (IntVal -v)
negateV (RealVal v) = (RealVal -v)

roundV :: Value -> Value
roundV (IntVal v) = (IntVal v)
roundV (RealVal v) = (IntVal (round v))

arithOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> Value
arithOp IntF FloatF (IntVal x) (IntVal y) = (IntVal ((IntF) x y))
arithOp IntF FloatF (RealVal x) (RealVal y) = (RealVal ((FloatF) x y))
arithOp IntF FloatF (IntVal x) (RealVal y) = (RealVal ((FloatF) (fromIntegral x) y))
arithOp IntF FloatF (RealVal x) (IntVal y) = (RealVal ((FloatF) x (fromIntegral y)))

multV :: Value -> Value -> Value
multV x y = arithOp (*) (*) x y

addV :: Value -> Value -> Value
addV x y = arithOp (+) (+) x y

powV :: Value -> Value -> Value
powV x y = arithOp (^) (**) x y

subV :: Value -> Value -> Value
subV x y = arithOp (-) (-) x y

modV :: Value -> Value -> Value
modV x y = arithOp (mod) (mod') x y

divV :: Value -> Value -> Value
divV x y = arithOp (/) (/) x y

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

isZero :: Value -> Boolean
isZero (IntVal x) = if (x == 0) then True else False
isZero (RealVal x) = if (x == 0.0) then True else False

factorial n = product [1..n]

fogarteHelper :: a -> a
fogarteHelper 0 = 0
fogarteHelper n = 
  let
    x = (((2*n) + 2) / (factorial ((2*n) + 1)))
    next = (fogarteHelper (n-1)) 
  in
    x + next

eval :: Expr -> Value
eval MultExpr Expr1 Expr2 = multV (eval Expr1) (eval Expr2)
eval AddExpr Expr1 Expr2 = addV (eval Expr1) (eval Expr2)
eval ExpExpr Expr1 Expr2 =  powV (eval Expr1) (eval Expr2)
eval SubExpr Expr1 Expr2 = subV (eval Expr1) (eval Expr2)
eval DivExpr Expr1 Expr2 = divV (eval Expr1) (eval Expr2)
eval IntDivExpr Expr1 Expr2 = intDivV (eval Expr1) (eval Expr2)
eval RoundExpr Expr = roundV (eval Expr)
eval ModExpr Expr1 Expr2 = modV (eval Expr1) (eval Expr2)
eval IfzThenElseExpr Expr1 Expr2 Expr3 = 
  let 
    res1 = (eval Expr1)
    res2 = (eval Expr2)
    res3 = (eval Expr3)
  in
    if (isZero res1) then res2 else res3
eval TauExpr = 2*pi
eval PiExpr = pi
eval FogarteExpr = (RealVal (fogarteHelper 6))
eval MSExpr Expr = (eval Expr)
eval MRExpr = 0
eval NegExpr Expr = negateV (eval Expr)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

}
