module Main where
import Language.Lisp
import Prelude

main :: IO ()
main = void . runLisp . forever $ do
  line <- liftIO getLine
  lisp line
