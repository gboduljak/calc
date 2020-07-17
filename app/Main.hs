module Main where
import Lexer (lexicallyAnalyse)
import Tokens (Token, Token(..))
import Ast (      
  Exp, Exp(..), 
  Exp', Exp'(..), 
  Term, Term(..), 
  Term',Term'(..),
  Factor, Factor(..),
  Atom, Atom(..),
  ArithOp, ArithOp(..))
import Parser (parse)
import Evaluator (evaluate)

main :: IO ()
main = do
  expression <- getLine
  maybeTokens <- return (lexicallyAnalyse expression)
  case maybeTokens of
    (Just tokens) -> do 
        maybeExp <- return (parse tokens)
        case maybeExp of 
          (Just exp) -> do
              result <- return (evaluate exp)
              putStrLn (show result)
          Nothing -> putStrLn "The input expression is not semantically valid!"
    Nothing -> putStrLn "The input expression is not lexically valid!"