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
            deriving (Show, Eq)
data Exp'   = NontrivialExp' { exp'Op :: ArithOp, exp'Term :: Term, exp'Exp' :: Exp'}
            | TrivialExp'
            deriving (Show, Eq)
data Term   = NontrivialTerm { termFactor :: Factor, termTerm' :: Term' } deriving (Show, Eq)
data Term'  = NontrivialTerm' { term'Op :: ArithOp, term'Factor :: Factor, term'Term' :: Term' }
            | TrivalTerm'
            deriving (Show, Eq)
data Factor = NegativeFactor { innerFactor :: Factor }
            | AtomicFactor { innerAtom :: Atom }
            deriving (Show, Eq)
data Atom   = NumericAtom { number :: Double }
            | ExpAtom { innerExp :: Exp }
            deriving (Show, Eq)

data ArithOp = Add | Sub | Mul | Div deriving (Show, Eq)
