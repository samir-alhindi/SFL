module Main where

import Text.Parsec
import Parsing
import AST
import Evalautor
import RuntimeData
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

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
            case my_parse source path of
                Left err         -> print err
                Right program'   -> do
                    let paths = import_paths program'
                    imports_source <- mapM readFile paths
                    case mapM parse_imports (zip paths imports_source) of
                        Left err -> print err
                        Right imports ->
                            let envi = global (program' ++ (concat imports)) in
                            start_repl envi

            where
                import_paths :: [Declaration] -> [String]
                import_paths [] = []
                import_paths ((TL_Import (Import file_path)):rest) = file_path : (import_paths rest)
                import_paths (_:xs) = import_paths xs
                
                parse_imports :: (String,String) -> Either ParseError [Declaration]
                parse_imports (path, source) = my_parse source path
            
                start_repl :: Environment -> IO ()
                start_repl envi = repl
                    where
                        repl :: IO ()
                        repl = do
                            putStr "> "
                            hFlush stdout
                            input <- getLine
                            if input == ":quit"
                                then return ()
                                else
                                    case parse (m_whiteSpace >> expression) "" input of
                                        Left err   -> print err >> repl
                                        Right expr -> case eval expr envi of
                                            Left err  -> print err >> repl
                                            Right val -> print val >> repl

