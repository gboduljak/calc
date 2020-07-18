module Lexer
    ( 
      lexicallyAnalyse
    ) 
where
import Data.Char (isDigit, isSpace)
import Data.List (zip)
import Data.Maybe
import Tokens (Token, Token(..))

data LexerState = Start     | 
                  Digits    |
                  Operators |
                  Parens    |
                  Error
                  deriving (Show)

isOperator :: Char -> Bool
isOperator x = x `elem` ['+','-','*','/'] 

isParens :: Char -> Bool
isParens x = x `elem` ['(',')'] 

lexicallyAnalyse :: String -> Maybe [Token]
lexicallyAnalyse input = consume input Start [] []

consume :: String -> LexerState -> [(LexerState, Char)] -> [Token] -> Maybe [Token]
consume (x:xs) Start history tokens
  | isDigit x    = consume xs Digits ((Start, x) : history) tokens
  | isOperator x = consume xs Operators ((Start, x) : history) tokens
  | isParens x   = consume xs Parens ((Parens, x) : history) tokens
  | isSpace x    = consume xs Start history tokens
  | otherwise    = Nothing

consume [] Digits history tokens = consume [] Start [] ((Number number):tokens)
  where number = read (historyAsStr history) :: Double
consume input@(x:xs) Digits history tokens
  | isDigit x = consume xs Digits ((Digits, x) : history) tokens
  | isSpace x = consume xs Digits history tokens
  | x == '.'  = consume xs Digits ((Digits, x) : history) tokens
  | otherwise = consume input Start [] ((Number number):tokens)
  where number = read (historyAsStr history) :: Double

consume [] Operators history tokens = consume [] Start [] ((Operator operator):tokens)
  where operator = (head . historyAsStr) history
consume input@(x:xs) Operators history tokens
  | isSpace x = consume xs Operators history tokens
  | otherwise = consume input Start [] ((Operator operator):tokens)
  where operator = (head . historyAsStr) history

consume [] Parens history tokens = consume [] Start [] (parensToken:tokens)
  where parens      = (head . historyAsStr) history
        parensToken = case parens of 
            '(' -> OpenParens
            ')' -> CloseParens
consume input Parens history tokens = consume input Start [] (parensToken:tokens)
  where parens      = (head . historyAsStr) history
        parensToken = case parens of 
            '(' -> OpenParens
            ')' -> CloseParens

consume [] state history tokens = Just (reverse tokens)
consume _ _ _ _ = Nothing

historyAsStr :: [(LexerState, Char)] -> String
historyAsStr history = reverse [ x | (state, x) <- history ]
