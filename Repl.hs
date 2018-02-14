module Repl where
import Lexer
import Parser
import Eval
import Data.Maybe
import Data.IORef

replInner :: IORef Value -> IO ()
replInner ms = do
  input <- getLine
  let tokens = (scanTokens input)
  case tokens of
    Just t -> do
      let stmt = (parse t)
      case stmt of
        Just s -> do
          case s of 
            Stmt (MRExpr) -> do
              memrecall <- readIORef ms
              case memrecall of
                (IntVal i) -> do
                  putStrLn $ (show i)
                  replInner ms
                (RealVal r) -> do
                  putStrLn $ (show r)
                  replInner ms
            Stmt e -> do
              evald <- (eval (Stmt e))
              case evald of
                (IntVal i) -> do
                  putStrLn $ (show i)
                  replInner ms
                (RealVal r) -> do
                  putStrLn $ (show r)
                  replInner ms
            MSStmt e -> do
              evald <- (eval (MSStmt e))
              writeIORef ms (evald)
              replInner ms
        Nothing -> do
          putStrLn $ "Oops, parse returned Nothing!"
          replInner ms
    Nothing -> do
      putStrLn $ "Oops, scanTokens returned Nothing!"
      replInner ms

repl :: IO ()
repl = do 
  memstore <- newIORef (IntVal 0)
  replInner memstore
