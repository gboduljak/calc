module Main where

import Lexer (lexicallyAnalyse)
import Tokens (Token, Token(..))
import Parser

main :: IO ()
main = do
  expression <- getLine
  tokens <- return (lexicallyAnalyse expression)
  result <- return (printLex tokens)
  putStrLn $ result

printLex :: Maybe [Token] -> String
printLex (Just tokens) = show tokens
printLex Nothing = "The input expression is not lexically valid!"