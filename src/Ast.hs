module Ast
    ( 
      Exp(..), 
      Exp'(..), 
      Term(..), 
      Term'(..),
      Factor(..),
      Atom(..),
      ArithOp(..)
    )
where
import Tokens (Token, Token(..))
data Exp    = NontrivialExp { expTerm :: Term, expExp' :: Exp' }
            | TrivialExp
            deriving (Show)
data Exp'   = NontrivialExp' { exp'Op :: ArithOp, exp'Term :: Term, exp'Exp' :: Exp'}
            | TrivialExp'
            deriving (Show)
data Term   = NontrivialTerm { termFactor :: Factor, termTerm' :: Term' } deriving (Show)
data Term'  = NontrivialTerm' { term'Op :: ArithOp, term'Factor :: Factor, term'Term' :: Term' }
            | TrivalTerm'
            deriving (Show)
data Factor = NegativeFactor { innerFactor :: Factor }
            | AtomicFactor { innerAtom :: Atom }
            deriving (Show)
data Atom   = NumericAtom { number :: Double }
            | ExpAtom { innerExp :: Exp }
            deriving (Show)

data ArithOp = Add | Sub | Mul | Div deriving (Show, Eq)