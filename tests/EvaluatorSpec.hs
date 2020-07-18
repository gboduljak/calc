module EvaluatorSpec (
  evaluatorSpec
) 
where
import Test.Hspec
import Data.Map
import Lexer (lexicallyAnalyse)
import Parser (parse)
import Evaluator (evaluate)
import Tokens (Token, Token(..))
import Ast (      
  Exp, Exp(..), 
  Exp', Exp'(..), 
  Term, Term(..), 
  Term',Term'(..),
  Factor, Factor(..),
  Atom, Atom(..),
  ArithOp, ArithOp(..))

lexed :: String -> [Token]
lexed = tokensOf . lexicallyAnalyse
  where tokensOf (Just tokens) = tokens
        tokensof Nothing = []

parsed :: [Token] -> Exp
parsed = expOf . parse
  where expOf (Just exp) = exp
        expOf Nothing = TrivialExp
  
tests :: Map Int (Exp, Double)
tests = fromList [
  (1, ((parsed . lexed) "1", 1.0)),
  (2, ((parsed . lexed) "- 1", -1.0)),
  (3, ((parsed . lexed) "- - ( 2 + 3 )", 5.0)),
  (4, ((parsed . lexed) "1.0 + 2.0 * 3.0 / ( 6.0*6.0 + 5.0*44.0)", 1.0234375)),
  (5, ((parsed . lexed) "1 + 2 * 3 ", 7.0)),
  (6, ((parsed . lexed) "1 * 2 + 3", 5.0)),
  (7, ((parsed . lexed) "-4 * (2 + 3)", -20.0)),
  (8, ((parsed . lexed) "1 + 2 + 3 + 4 + 5 + 6 + 7 ", 28.0)),
  (9, ((parsed . lexed) "1 +2 -3 +4 -5 +6 + 7 +8 + 9-10+  11", 30.0)),
  (10, ((parsed . lexed) "1 -2 + 4 - 2 *5 +3 * 6 *7+4 - 134 *5 ", -547.0)),
  (11, ((parsed . lexed) " ( 1 + 2) * (2 + 3) + (4 * 5) + (6 * 7) / ((5 -2) * (2 +4))", 37.33333333333333)),
  (12, ((parsed . lexed) "(1 + 2) * (5 - (4 / 2))", 9.0)),
  (13, ((parsed . lexed) "- (3 + (4 / 2)) * (- (4 + 2*4 - 1))", 55.0)),
  (14, ((parsed . lexed) "(((3 - 2 + 3)/ (2 + 1) + 2 - 4)*(4/ (2 + 1)))", -0.88888888888888)),
  (15, ((parsed . lexed) "((2 * 3 + 1) * (1 + 2 -5)) + 4", -10.0)),
  (16, ((parsed . lexed) "1 + 2 - 3 - 4", -4.0))]

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "evaluator tests ..." $ do
    it "correctly evaluates #1" $ do
      evaluate (fst (tests ! 1)) `shouldBe` (snd (tests ! 1))
    it "correctly evaluates #2" $ do
      evaluate (fst (tests ! 2)) `shouldBe` (snd (tests ! 2))
    it "correctly evaluates #3" $ do
        evaluate (fst (tests ! 3)) `shouldBe` (snd (tests ! 3))
    it "correctly evaluates #4" $ do
      evaluate (fst (tests ! 4)) `shouldBe` (snd (tests ! 4))
    it "correctly evaluates #5" $ do
      evaluate (fst (tests ! 5)) `shouldBe` (snd (tests ! 5))
    it "correctly evaluates #6" $ do
      evaluate (fst (tests ! 6)) `shouldBe` (snd (tests ! 6))
    it "correctly evaluates #7" $ do
      evaluate (fst (tests ! 7)) `shouldBe` (snd (tests ! 7))
    it "correctly evaluates #8" $ do
      evaluate (fst (tests ! 8)) `shouldBe` (snd (tests ! 8))
    it "correctly evaluates #9" $ do
      evaluate (fst (tests ! 9)) `shouldBe` (snd (tests ! 9))
    it "correctly evaluates #10" $ do
      evaluate (fst (tests ! 10)) `shouldBe` (snd (tests ! 10))
    it "correctly evaluates #11" $ do
      evaluate (fst (tests ! 11)) `shouldBe` (snd (tests ! 11))
    it "correctly evaluates #12" $ do
      evaluate (fst (tests ! 12)) `shouldBe` (snd (tests ! 12))
    it "correctly evaluates #13" $ do
      evaluate (fst (tests ! 13)) `shouldBe` (snd (tests ! 13))
    it "correctly evaluates #14" $ do
      evaluate (fst (tests ! 14)) `shouldBe` (snd (tests ! 14))
    it "correctly evaluates #15" $ do
      evaluate (fst (tests ! 15)) `shouldBe` (snd (tests ! 15))
    it "correctly evaluates #16" $ do
      evaluate (fst (tests ! 16)) `shouldBe` (snd (tests ! 16))