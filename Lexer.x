{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$eol = [\n]

@integer = $digit+
@real = ($digit*\.$digit+) 

tokens :-

    $white+         ;
    $eol            ;
    @integer        { \s -> IntTok (read s) }
    @real           { \s -> RealTok (read s) }
    \+              { \s -> AddTok }
    \-              { \s -> SubTok }
    \/              { \s -> DivTok }
    \/\/            { \s -> IntDivTok }
    \^              { \s -> ExpTok }
    life            { \s -> LifeTok }
    pi              { \s -> PiTok }
    fogarte         { \s -> FogarteTok }
    tau             { \s -> TauTok }
    \~              { \s -> RoundTok }
    \*              { \s -> MultTok }
    \%              { \s -> ModTok }
    \(              { \s -> LParenTok }
    \)              { \s -> RParenTok }
    ifz             { \s -> IfzTok }
    then            { \s -> ThenTok }
    else            { \s -> ElseTok }
    MS              { \s -> MSTok }
    MR              { \s -> MRTok }
    \?              { \s -> DoneTok }
    
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    IntTok Int      |
    RealTok Float   |
    MultTok         |
    AddTok          |
    ExpTok          |
    SubTok          |
    DivTok          |
    IntDivTok       |
    LifeTok         |
    PiTok           |
    FogarteTok      |
    TauTok          |
    RoundTok        |
    ModTok          |
    LParenTok       |
    RParenTok       |
    IfzTok          |
    ThenTok         |
    ElseTok         |
    MSTok           |
    MRTok           |
    DoneTok
    deriving (Eq,Show)

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
--main = do
--  s <- getContents
--  print (scanTokens s)
}
