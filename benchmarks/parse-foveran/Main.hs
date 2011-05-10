module Main where

import           System.Exit
import           System.IO
import           System.Environment
import           Text.PrettyPrint
import qualified Parsing as P

data Action
    = Parse    FilePath
    | Lex      FilePath

parseArgs :: IO Action
parseArgs = getArgs >>= parse
    where
      parse [ "lex", fnm ]        = return $ Lex fnm
      parse [ fnm ]               = return $ Parse fnm
      parse _ = do
        hPutStrLn stderr "Usage: "
        hPutStrLn stderr "  parse-benchmark lex <filename>.fv"
        hPutStrLn stderr "  parse-benchmark <filename>.fv"
        exitFailure

main :: IO ()
main = do
  action <- parseArgs
  case action of
    Lex filename ->
        do readResult <- P.lexFoveranFile filename
           case readResult of 
             Left err ->
                 do hPutStrLn stderr $ render (P.ppInputError err)
                    exitFailure
             Right tokens ->
                 do exitSuccess
    Parse filename ->
        do readResult <- P.readFoveranFile2 filename
           case readResult of 
             Left err ->
                 do hPutStrLn stderr $ render (P.ppInputError err)
                    exitFailure
             Right decls ->
                 do putStrLn $ render (P.prettyKnot decls)
                    exitSuccess
