{
module Parser where
import Lexer
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
}

%monad { Maybe } 
%name parse
%tokentype { Token }
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

%left '?' 
%left '+' '-' 
%left '*' '/' '//' '%' 
%left '^'
%right ifz then else ms
%nonassoc NEG ROUND
%%

P : S '?'                       { $1 }

S :: { Stmt }
S : E                           { Stmt $1 }
  | E ms                        { MSStmt $1 }

E :: { Expr }
E : '(' E ')'                   { $2 }
  | mr                          { MRExpr }
  | pi                          { PiExpr }
  | life                        { NumExpr (IntVal 42) }
  | fogarte                     { FogarteExpr }
  | int                         { NumExpr (IntVal $1) }
  | real                        { NumExpr (RealVal $1) }
  | tau                         { TauExpr }
  | E '?'                       { DoneExpr $1 }
  | E '+' E                     { AddExpr $1 $3 }
  | E '-' E                     { SubExpr $1 $3 }
  | E '*' E                     { MultExpr $1 $3 }
  | E '/' E                     { DivExpr $1 $3 }
  | E '//' E                    { IntDivExpr $1 $3 }
  | E '%' E                     { ModExpr $1 $3 }
  | E '^' E                     { ExpExpr $1 $3 }
  | ifz E then E else E         { IfzThenElseExpr $2 $4 $6 }
  | E ms                        { MSExpr $1 }
  | '-' E %prec NEG             { NegExpr $2 }
  | '~' E %prec ROUND           { RoundExpr $2 }

{

data Value = IntVal Int | RealVal Float deriving (Show, Eq)

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
  MRExpr                          |
  NumExpr Value                   |
  NegExpr Expr                    |
  DoneExpr Expr
  deriving (Show, Eq)

data Stmt = 
  Stmt Expr     |
  MSStmt Expr   |
  deriving (Show, Eq)

data Pgrm =
  Pgrm Stmt
  deriving (Show, Eq)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

}
