module Main where

import Parsing
import Evalautor
import System.Environment (getArgs)

run :: IO ()
run = do
    args <- getArgs
    if length args == 0
        then helper "C:\\Users\\Samir\\Desktop\\code\\haskell\\sfl_interpreter\\programs\\debug.sfl"
        else helper (args !! 0)

    where
        helper :: String -> IO ()
        helper path = do
            source <- (readFile path)
            case my_parse source of
                Left err         -> print err
                Right program'    -> case exec_program program' of
                    Left err -> print err
                    Right io -> io

main :: IO ()
main = run
