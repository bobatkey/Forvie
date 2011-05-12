module Main where

import Control.Applicative
import Parsing
import Criterion.Main

foo :: Either a b -> Either () ()
foo (Left _) = Left ()
foo (Right _) = Right ()

main = defaultMain [ bench "inline-lex" $ whnfIO (foo <$> readFoveranFile "inductors-descript.fv")
                   , bench "sep-lex" $ whnfIO (foo <$> readFoveranFile2 "inductors-descript.fv")
                   , bench "text-lex" $ whnfIO (foo <$> readFoveranFile3 "inductors-descript.fv")
                   , bench "lex" $ whnfIO (foo <$> lexFoveranFile "inductors-descript.fv")
                   , bench "lex2" $ whnfIO (lexFoveranFile2 "inductors-descript.fv")
                   , bench "lex3" $ whnfIO (lexFoveranFile3 "inductors-descript.fv")
                   ]