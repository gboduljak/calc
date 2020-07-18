module Parser
    ( 
      parse
    ) 
where
import Data.List (tail)
import Tokens (Token, Token(..))
import Ast (      
  Exp, Exp(..), 
  Exp', Exp'(..), 
  Term, Term(..), 
  Term',Term'(..),
  Factor, Factor(..),
  Atom, Atom(..),
  ArithOp, ArithOp(..))

parse :: [Token] -> Maybe Exp 
parse tokens 
  | null remainingTokens  = exp
  | otherwise             = Nothing
  where (exp, remainingTokens) = parseExp tokens 

parseExp :: [Token] -> (Maybe Exp, [Token])
parseExp [] = (Just TrivialExp,[])
parseExp tokens@(lookahead : rest)
  | (Operator op)       <- lookahead  = parse' op
  | (Tokens.Number num) <- lookahead  = parse' 'n'
  | OpenParens          <- lookahead  = parse' '('
  | otherwise                         = (Nothing, tokens)
  where parse' look | look == '+' = (liftIntoExp term exp', resultRest)
                    | look == '-' = (liftIntoExp term exp', resultRest)
                    | look == '(' = (liftIntoExp term exp', resultRest)
                    | look == 'n' = (liftIntoExp term exp', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (term, termRest)   = parseTerm tokens
                          (exp', resultRest) = parseExp' termRest

liftIntoExp :: Maybe Term -> Maybe Exp' -> Maybe Exp
liftIntoExp (Just term) (Just exp') = Just exp
  where exp = NontrivialExp {
    expTerm = term,
    expExp' = exp'
  }
liftIntoExp _ _ = Nothing

parseTerm :: [Token] -> (Maybe Term, [Token])
parseTerm [] = (Nothing, [])
parseTerm tokens@(lookahead : rest)
  | (Operator op)       <- lookahead  = parse' op
  | (Tokens.Number num) <- lookahead  = parse' 'n'
  | OpenParens          <- lookahead  = parse' '('
  | otherwise                         = (Nothing, tokens)
  where parse' look | look == '-' = (liftIntoTerm factor term', resultRest)
                    | look == 'n' = (liftIntoTerm factor term', resultRest)
                    | look == '(' = (liftIntoTerm factor term', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor tokens
                          (term', resultRest)  = parseTerm' factorRest

liftIntoTerm :: Maybe Factor -> Maybe Term' -> Maybe Term
liftIntoTerm (Just factor) (Just term') = Just term
  where term = NontrivialTerm {
    termFactor = factor,
    termTerm' = term'
  }
liftIntoTerm _ _ = Nothing

parseExp' :: [Token] -> (Maybe Exp', [Token])
parseExp' [] = (Just TrivialExp', [])
parseExp' tokens@(lookahead : rest)
  | (Operator op) <- lookahead  = parse' op
  | CloseParens   <- lookahead  = (Just TrivialExp', tokens)
  | otherwise                   = (Nothing, tokens)
  where parse' look | look == '+' = (liftIntoExp' Add term exp', resultRest)
                    | look == '-' = (liftIntoExp' Sub term exp', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (term, termRest)   = parseTerm rest
                          (exp', resultRest) = parseExp' termRest

liftIntoExp' :: ArithOp -> Maybe Term -> Maybe Exp' -> Maybe Exp'
liftIntoExp' op (Just term) (Just exp') = Just exp''
  where exp'' = NontrivialExp' {
    exp'Op  = op,
    exp'Term = term,
    exp'Exp' = exp'
  }
liftIntoExp' _ _ _ = Nothing

parseTerm' :: [Token] -> (Maybe Term', [Token])
parseTerm' [] = (Just TrivalTerm', [])
parseTerm' tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | CloseParens   <- lookahead = (Just TrivalTerm', tokens)
  | otherwise                  = (Nothing, tokens)
  where parse' look | look == '+' = (Just TrivalTerm', tokens)
                    | look == '-' = (Just TrivalTerm', tokens)
                    | look == '*' = (liftIntoTerm' Mul factor term', resultRest)
                    | look == '/' = (liftIntoTerm' Div factor term', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor rest
                          (term', resultRest)  = parseTerm' factorRest

liftIntoTerm' :: ArithOp -> Maybe Factor -> Maybe Term' -> Maybe Term'
liftIntoTerm' op (Just factor) (Just term') = Just term''
  where term'' = NontrivialTerm' {
    term'Op  = op,
    term'Factor = factor,
    term'Term' = term'
  }
liftIntoTerm' _ _ _ = Nothing

parseFactor :: [Token] -> (Maybe Factor, [Token])
parseFactor [] = (Nothing, [])
parseFactor tokens@(lookahead : rest)
  | (Operator op)       <- lookahead  = parse' op
  | (Tokens.Number num) <- lookahead  = parse' 'n'
  | OpenParens          <- lookahead  = parse' '('
  | otherwise                         = (Nothing, tokens)
  where parse' look | look == '-' = (liftIntoNegativeFactor factor, factorRest)
                    | look == 'n' = (liftIntoAtomicFactor atom, atomRest)
                    | look == '(' = (liftIntoAtomicFactor atom, atomRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor rest
                          (atom, atomRest)     = parseAtom tokens 

liftIntoNegativeFactor :: Maybe Factor -> Maybe Factor
liftIntoNegativeFactor (Just factor) = Just NegativeFactor { innerFactor = factor }
liftIntoNegativeFactor Nothing       = Nothing

liftIntoAtomicFactor :: Maybe Atom -> Maybe Factor
liftIntoAtomicFactor (Just atom) = Just AtomicFactor { innerAtom = atom }
liftIntoAtomicFactor Nothing     = Nothing

parseAtom :: [Token] -> (Maybe Atom, [Token])
parseAtom [] = (Nothing, [])
parseAtom tokens@(lookahead : rest)
  | (Tokens.Number value) <- lookahead = parse' 'n'
  | OpenParens            <- lookahead = parse' '('
  | otherwise                          = (Nothing, tokens)
  where parse' look | look == 'n'                  = (liftIntoNumericAtom value, rest)
                    | look == '(' && expRest /= [] = (liftIntoExpAtom exp, tail expRest)
                    | otherwise                    = (Nothing, tokens)
                    where (exp, expRest)        = parseExp rest
                          (Tokens.Number value) = lookahead

liftIntoNumericAtom :: Double -> Maybe Atom
liftIntoNumericAtom value = Just NumericAtom { number = value }

liftIntoExpAtom :: Maybe Exp -> Maybe Atom
liftIntoExpAtom (Just exp) = Just ExpAtom { innerExp = exp }
liftIntoExpAtom _          = Nothing
