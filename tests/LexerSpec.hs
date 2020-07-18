module LexerSpec (
  lexerSpec
) 
where
import Test.Hspec
import Data.Map
import Tokens (Token, Token(..))
import Lexer (lexicallyAnalyse)

tests :: Map Int (String, Maybe [Token])
tests = fromList [
  (1, ("1.0 + 2.0 * 3.0 / ( 6.0*6.0 + 5.0*44.0)", Just [Number 1.0,Operator '+',Number 2.0,Operator '*',Number 3.0,Operator '/',OpenParens,Number 6.0,Operator '*',Number 6.0,Operator '+',Number 5.0,Operator '*',Number 44.0,CloseParens])),
  (2, ("123+7-10*8-10+9*(4-3)/4", Just [Number 123.0,Operator '+',Number 7.0,Operator '-',Number 10.0,Operator '*',Number 8.0,Operator '-',Number 10.0,Operator '+',Number 9.0,Operator '*',OpenParens,Number 4.0,Operator '-',Number 3.0,CloseParens,Operator '/',Number 4.0])),
  (3, ("-10*7+4-10", Just [Operator '-',Number 10.0,Operator '*',Number 7.0,Operator '+',Number 4.0,Operator '-',Number 10.0])),
  (4, ("1-(200-99.5*2)", Just [Number 1.0,Operator '-',OpenParens,Number 200.0,Operator '-',Number 99.5,Operator '*',Number 2.0,CloseParens])),
  (5, (" (2 + (2/34) *231 ) / (232 + (2 * (2+5)*(4/8)))", Just [OpenParens,Number 2.0,Operator '+',OpenParens,Number 2.0,Operator '/',Number 34.0,CloseParens,Operator '*',Number 231.0,CloseParens,Operator '/',OpenParens,Number 232.0,Operator '+',OpenParens,Number 2.0,Operator '*',OpenParens,Number 2.0,Operator '+',Number 5.0,CloseParens,Operator '*',OpenParens,Number 4.0,Operator '/',Number 8.0,CloseParens,CloseParens,CloseParens])),
  (6, ("(38+2*14-(21 / 7 +(23 - 1))*(4) + (((11 * 14*100 - 1 - (3) * 11-3))))/(2+3)", Just [OpenParens,Number 38.0,Operator '+',Number 2.0,Operator '*',Number 14.0,Operator '-',OpenParens,Number 21.0,Operator '/',Number 7.0,Operator '+',OpenParens,Number 23.0,Operator '-',Number 1.0,CloseParens,CloseParens,Operator '*',OpenParens,Number 4.0,CloseParens,Operator '+',OpenParens,OpenParens,OpenParens,Number 11.0,Operator '*',Number 14.0,Operator '*',Number 100.0,Operator '-',Number 1.0,Operator '-',OpenParens,Number 3.0,CloseParens,Operator '*',Number 11.0,Operator '-',Number 3.0,CloseParens,CloseParens,CloseParens,CloseParens,Operator '/',OpenParens,Number 2.0,Operator '+',Number 3.0,CloseParens])),
  (7, ("(3*(11)+(2)-3)", Just [OpenParens,Number 3.0,Operator '*',OpenParens,Number 11.0,CloseParens,Operator '+',OpenParens,Number 2.0,CloseParens,Operator '-',Number 3.0,CloseParens])),
  (8, ("-4.123*(-1)/(-10*(-2.1)^(-2))", Nothing)),
  (9, ("-.31323+()", Nothing)),
  (10, ("(1 + 2) * (5 - (4 / 2))", Just [OpenParens,Number 1.0,Operator '+',Number 2.0,CloseParens,Operator '*',OpenParens,Number 5.0,Operator '-',OpenParens,Number 4.0,Operator '/',Number 2.0,CloseParens,CloseParens])),
  (11, ("50*87.-4", Nothing))]

lexerSpec :: Spec
lexerSpec = do
  describe "lexer tests ..." $ do
    it "correctly lexically analyses #1" $ do
      lexicallyAnalyse (fst (tests ! 1)) `shouldBe` (snd (tests ! 1))
    it "correctly lexically analyses #2" $ do
      lexicallyAnalyse (fst (tests ! 2)) `shouldBe` (snd (tests ! 2))
    it "correctly lexically analyses #3" $ do
      lexicallyAnalyse (fst (tests ! 3)) `shouldBe` (snd (tests ! 3))
    it "correctly lexically analyses #4" $ do
      lexicallyAnalyse (fst (tests ! 4)) `shouldBe` (snd (tests ! 4))
    it "correctly lexically analyses #5" $ do
      lexicallyAnalyse (fst (tests ! 5)) `shouldBe` (snd (tests ! 5))
    it "correctly lexically analyses #6" $ do
      lexicallyAnalyse (fst (tests ! 6)) `shouldBe` (snd (tests ! 6))
    it "correctly lexically analyses #7" $ do
      lexicallyAnalyse (fst (tests ! 7)) `shouldBe` (snd (tests ! 7))
    it "correctly lexically analyses #8" $ do
      lexicallyAnalyse (fst (tests ! 8)) `shouldBe` (snd (tests ! 8))
    it "correctly lexically analyses #9" $ do
      lexicallyAnalyse (fst (tests ! 9)) `shouldBe` (snd (tests ! 9))
    it "correctly lexically analyses #10" $ do
      lexicallyAnalyse (fst (tests ! 10)) `shouldBe` (snd (tests ! 10))
    it "correctly lexically analyses #11" $ do
      lexicallyAnalyse (fst (tests ! 11)) `shouldBe` (snd (tests ! 11))
