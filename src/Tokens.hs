module Tokens
    ( 
      Token(..)
    ) 
where
data Token = Operator Char | 
             Number Double |  
             OpenParens    | 
             CloseParens 
      deriving (Show)