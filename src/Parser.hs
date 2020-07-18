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
  | remainingTokens == [] = exp
  | otherwise = Nothing
  where (exp, remainingTokens) = parseExp tokens 

parseExp :: [Token] -> (Maybe Exp, [Token])
parseExp [] = (Just TrivialExp,[])
parseExp tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | (Tokens.Number num) <- lookahead = parse' 'n'
  | OpenParens <- lookahead = parse' '('
  | otherwise =  (Nothing, tokens)
  where parse' look | look == '+' = (makeExp term exp', resultRest)
                    | look == '-' = (makeExp term exp', resultRest)
                    | look == '(' = (makeExp term exp', resultRest)
                    | look == 'n' = (makeExp term exp', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (term, termRest)   = parseTerm tokens
                          (exp', resultRest) = parseExp' termRest

makeExp :: Maybe Term -> Maybe Exp' -> Maybe Exp
makeExp (Just term) (Just exp') = Just exp
  where exp = NontrivialExp { 
    expTerm = term, 
    expExp' = exp' 
  }
makeExp _ _ = Nothing

parseTerm :: [Token] -> (Maybe Term, [Token])
parseTerm [] = (Nothing, [])
parseTerm tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | (Tokens.Number num) <- lookahead = parse' 'n'
  | OpenParens <- lookahead = parse' '('
  | otherwise =  (Nothing, tokens)
  where parse' look | look == '-' = (makeTerm factor term', resultRest)
                    | look == 'n' = (makeTerm factor term', resultRest)
                    | look == '(' = (makeTerm factor term', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor tokens
                          (term', resultRest)  = parseTerm' factorRest

makeTerm :: Maybe Factor -> Maybe Term' -> Maybe Term
makeTerm (Just factor) (Just term') = Just term
  where term = NontrivialTerm {
    termFactor = factor,
    termTerm' = term'
  }
makeTerm _ _ = Nothing

parseExp' :: [Token] -> (Maybe Exp', [Token])
parseExp' [] = (Just TrivialExp', [])
parseExp' tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | CloseParens <- lookahead = (Just TrivialExp', tokens)
  | otherwise = (Nothing, tokens)
  where parse' look | look == '+' = (makeExp' Add term exp', resultRest)
                    | look == '-' = (makeExp' Sub term exp', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (term, termRest)   = parseTerm rest
                          (exp', resultRest) = parseExp' termRest

makeExp' :: ArithOp -> Maybe Term -> Maybe Exp' -> Maybe Exp'
makeExp' op (Just term) (Just exp') = Just exp''
  where exp'' = NontrivialExp' {
    exp'Op  = op,
    exp'Term = term,
    exp'Exp' = exp'
  }
makeExp' _ _ _ = Nothing

parseTerm' :: [Token] -> (Maybe Term', [Token])
parseTerm' [] = (Just TrivalTerm', [])
parseTerm' tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | CloseParens <- lookahead = (Just TrivalTerm', tokens)
  | otherwise = (Nothing, tokens)
  where parse' look | look == '+' = (Just TrivalTerm', tokens)
                    | look == '-' = (Just TrivalTerm', tokens)
                    | look == '*' = (makeTerm' Mul factor term', resultRest)
                    | look == '/' = (makeTerm' Div factor term', resultRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor rest
                          (term', resultRest)  = parseTerm' factorRest

makeTerm' :: ArithOp -> Maybe Factor -> Maybe Term' -> Maybe Term'
makeTerm' op (Just factor) (Just term') = Just term''
  where term'' = NontrivialTerm' {
    term'Op  = op,
    term'Factor = factor,
    term'Term' = term'
  }
makeTerm'  _ _ _ = Nothing

parseFactor :: [Token] -> (Maybe Factor, [Token])
parseFactor [] = (Nothing, [])
parseFactor tokens@(lookahead : rest)
  | (Operator op) <- lookahead = parse' op
  | (Tokens.Number num) <- lookahead = parse' 'n'
  | OpenParens <- lookahead = parse' '('
  | otherwise = (Nothing, tokens)
  where parse' look | look == '-' = (liftNegativeFactor factor, factorRest)
                    | look == 'n' = (liftAtomicFactor atom, atomRest)
                    | look == '(' = (liftAtomicFactor atom, atomRest)
                    | otherwise   = (Nothing, tokens)
                    where (factor, factorRest) = parseFactor rest
                          (atom, atomRest) = parseAtom tokens 

liftNegativeFactor :: Maybe Factor -> Maybe Factor
liftNegativeFactor (Just factor) = Just NegativeFactor { innerFactor = factor }
liftNegativeFactor Nothing = Nothing

liftAtomicFactor :: Maybe Atom -> Maybe Factor
liftAtomicFactor (Just atom) = Just AtomicFactor { innerAtom = atom }
liftAtomicFactor Nothing = Nothing

parseAtom :: [Token] -> (Maybe Atom, [Token])
parseAtom tokens@(lookahead : rest)
  | (Tokens.Number value) <- lookahead = parse' 'n'
  | OpenParens <- lookahead = parse' '('
  | otherwise = (Nothing, tokens)
  where parse' look | look == 'n'                  = (liftNumericAtom value, rest)
                    | look == '(' && expRest /= [] = (liftExpAtom exp, tail expRest)
                    | otherwise   = (Nothing, tokens)
                    where (exp, expRest)  = parseExp rest
                          (Tokens.Number value) = lookahead
parseAtom tokens = (Nothing, tokens)

liftNumericAtom :: Double -> Maybe Atom
liftNumericAtom value = Just NumericAtom { number = value }

liftExpAtom :: Maybe Exp -> Maybe Atom
liftExpAtom (Just exp) = Just ExpAtom { innerExp = exp }
liftExpAtom _          = Nothing
