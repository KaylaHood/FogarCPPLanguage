{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
    $alpha [$alpha $digit]*     { \s -> VarTok s }
    let         { \s -> LetTok }
    $white+     ;
    in          { \s -> InTok }
    $digit+     { \s -> IntTok (read s) }
    \=        { \s -> EqTok }


{
-- Each action has type :: String -> Token

-- The token type:
data Token = LetTok | IntTok Int | EqTok | VarTok String | OParenTok
           | CParenTok | PlusTok | DoTok | DoneTok | InTok deriving Show

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')
main = do
  s <- getContents
  print (scanTokens s)
}
