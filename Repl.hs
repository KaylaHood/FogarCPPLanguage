module Repl where
import Lexer
import Parser
import Eval
import Data.Maybe

repl :: IO ()
repl = do 
    input <- getLine
    let tokens = (scanTokens input)
    case tokens of
        Just t -> do
            let parsed = (parse t)
            case parsed of
                Just p -> do
                    let evald = (eval p)
                    case evald of
                        (IntVal i) -> putStrLn $ (show i)
                        (RealVal r) -> putStrLn $ (show r)
                Nothing -> putStrLn $ "Oops, parse returned Nothing!"
        Nothing -> putStrLn $ "Oops, scanTokens returned Nothing!"
