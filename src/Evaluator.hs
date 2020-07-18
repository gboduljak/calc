module Evaluator
    ( 
      evaluate
    ) 
where
import Data.List (head, tail)
import Tokens (Token, Token(..))
import Ast (      
  Exp, Exp(..), 
  Exp', Exp'(..), 
  Term, Term(..), 
  Term',Term'(..),
  Factor, Factor(..),
  Atom, Atom(..),
  ArithOp, ArithOp(..))

evaluate :: Exp -> Double
evaluate TrivialExp = 0
evaluate NontrivialExp { 
  expTerm = term, 
  expExp' = exp'
} = evaluateTerm term + evaluateExp' exp'

evaluateExp' :: Exp' -> Double
evaluateExp' TrivialExp' = 0
evaluateExp' NontrivialExp' { 
  exp'Op = op, 
  exp'Term = term, 
  exp'Exp' = rest 
} 
  | op == Add = evaluateTerm term + evaluateExp' rest
  | op == Sub = - evaluateTerm term + evaluateExp' rest

evaluateTerm :: Term -> Double
evaluateTerm NontrivialTerm { 
  termFactor = factor, 
  termTerm' = term' 
} = evaluateTerm' (evaluateFactor factor) term'

evaluateTerm' :: Double -> Term' -> Double
evaluateTerm' x TrivalTerm' = x
evaluateTerm' x NontrivialTerm' { 
  term'Op = op, 
  term'Factor = factor, 
  term'Term' = term' 
} 
  | op == Mul = evaluateTerm' (x * evaluateFactor factor) term'
  | op == Div = evaluateTerm' (x / evaluateFactor factor) term'

evaluateFactor :: Factor -> Double
evaluateFactor NegativeFactor { innerFactor = factor } = - (evaluateFactor factor)
evaluateFactor AtomicFactor { innerAtom = atom }       = evaluateAtom atom

evaluateAtom :: Atom -> Double
evaluateAtom NumericAtom { number = value } = value
evaluateAtom ExpAtom { innerExp = exp }     = evaluate exp