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
evaluate NontrivialExp { expTerm = term, expExp' = exp' } 
  | isTrivialExp' exp'  = (evaluateTerm term)
  | op == Add           = (evaluateTerm term) + (evaluate innerExp)
  | op == Sub           = (evaluateTerm term) - (evaluate innerExp)
  where op       = getExpOp exp'
        innerExp = liftExp  exp'

evaluateTerm :: Term -> Double
evaluateTerm NontrivialTerm { termFactor = factor, termTerm' = term' } 
  | isTrivialTerm' term' = (evaluateFactor factor)
  | op == Mul            = (evaluateFactor factor) * (evaluateTerm innerTerm)
  | op == Div            = (evaluateFactor factor) / (evaluateTerm innerTerm)
  where op        = getTermOp term'
        innerTerm = liftTerm term'

evaluateFactor :: Factor -> Double
evaluateFactor NegativeFactor { innerFactor = factor } = - (evaluateFactor factor)
evaluateFactor AtomicFactor { innerAtom = atom } = evaluateAtom atom

evaluateAtom :: Atom -> Double
evaluateAtom NumericAtom { number = value } = value
evaluateAtom ExpAtom { innerExp = exp } = evaluate exp

getExpOp :: Exp' -> ArithOp
getExpOp NontrivialExp' { 
    exp'Op = op, 
    exp'Term = _, 
    exp'Exp' = _ 
} = op

getTermOp :: Term' -> ArithOp
getTermOp NontrivialTerm' { 
  term'Op = op, 
  term'Factor = _, 
  term'Term' = _
} = op

liftExp :: Exp' -> Exp
liftExp NontrivialExp' { 
    exp'Op = _, 
    exp'Term = term , 
    exp'Exp' = exp' 
} = NontrivialExp { expTerm = term, expExp' = exp' } 

liftTerm :: Term' -> Term
liftTerm NontrivialTerm' {
  term'Op = _, 
  term'Factor = factor, 
  term'Term' = term'
} = NontrivialTerm { termFactor = factor, termTerm' = term' } 

isTrivialExp' :: Exp' -> Bool
isTrivialExp' TrivialExp' = True
isTrivialExp' _           = False

isTrivialTerm' :: Term' -> Bool
isTrivialTerm' TrivalTerm' = True
isTrivialTerm' _           = False