module Main where

import Text.Parsec
import Parsing
import AST
import Evalautor
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then run "C:\\Users\\Samir\\Desktop\\code\\haskell\\sfl_interpreter\\programs\\debug2.sfl"
        else run (args !! 0)

    where
        run :: String -> IO ()
        run path = do
            source <- readFile path
            case my_parse source of
                Left err         -> print err
                Right program'   -> do
                    imports_source <- mapM readFile (import_paths program')
                    case mapM parse_imports imports_source of
                        Left err -> print err
                        Right imports -> case exec_program program' (concat imports) of
                            Left err -> print err
                            Right values -> mapM_ (putStrLn . show) values

            where
                import_paths :: [Declaration] -> [String]
                import_paths [] = []
                import_paths ((TL_Import (Import file_path)):rest) = file_path : (import_paths rest)
                import_paths (_:xs) = import_paths xs
                
                parse_imports :: String -> Either ParseError [Declaration]
                parse_imports source = do
                    case my_parse source of
                        Left err       -> Left err
                        Right program' -> Right program'