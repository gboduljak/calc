import Test.Hspec
import LexerSpec (lexerSpec)
import ParserSpec (parserSpec)
import EvaluatorSpec (evaluatorSpec)

main :: IO ()
main = do
  hspec lexerSpec
  hspec parserSpec
  hspec evaluatorSpec
