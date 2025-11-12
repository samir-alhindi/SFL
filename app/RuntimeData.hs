module RuntimeData where

import AST
import Text.Parsec
import Text.Printf
import Data.List (isSuffixOf)

data Value = 
      Number' {get_num :: Double}
    | Boolean' {get_bool :: Bool}
    | String' {get_str :: String}
    | Lambda' [String] Expr Environment
    | Function' String [String] Expr Environment
    | List' [Value]

data MonadType = Right' | Left' | Just' | Nothing' deriving(Eq)

instance Eq Value where
    (Number' n1)  == (Number' n2) = n1 == n2
    (Boolean' b1) == (Boolean' b2) = b1 == b2
    (String' s1)  == (String' s2) = s1 == s2
    (Lambda' _ _ _ )  == (Lambda' _ _ _ ) = False
    (Function' name1 _ _ _ ) == (Function' name2 _ _ _ ) = name1 == name2
    (List' l1) == (List' l2) = l1 == l2
    _ == _ = False

data Error' = Error' String SourcePos | Error'' String

instance Show Error' where
    show (Error' err_log pos) =
        printf "Error at position (%d,%d): %s" (sourceLine pos) (sourceColumn pos) err_log
    show (Error'' err_log) = err_log

instance Show Value where
    show (Number' n) = if ".0" `isSuffixOf` (show n) then show (floor n :: Integer) else show n
    show (Boolean' b  ) = show b
    show (String' s) = show s
    show (Lambda' _ _ _ ) = "lambda"
    show (Function' name _ _ _ ) = "function " ++ name
    show (List' elements) = show elements
    

type Map = [(String, Value)]

data Environment =
      Global {get_map :: Map}
    | Environment {get_map :: Map, get_outer :: Environment}
    deriving (Eq)

find :: Environment -> String -> SourcePos -> Either Error' Value
find envi name pos = case envi of
    (Global map') -> case filter p map' of
        []   -> Left (Error' ("unbound variable: " ++ name) pos)
        list -> let (_, value) = head list in Right value
    (Environment map' outer) -> case filter p map' of
        []   -> find outer name pos
        list -> let (_, value) = head list in Right value
    where
        p :: (String, Value) -> Bool
        p (k, _) = k == name

find' :: Environment -> String -> Either Error' Value
find' envi name = case envi of
    (Global map') -> case filter p map' of
        []   -> Left (Error'' ("unbound variable: " ++ name))
        list -> let (_, value) = head list in Right value
    (Environment map' outer) -> case filter p map' of
        []   -> find' outer name
        list -> let (_, value) = head list in Right value
    where
        p :: (String, Value) -> Bool
        p (k, _) = k == name

extend_envi :: Environment -> (String, Value) -> Environment
extend_envi (Global map')             pair = Global (pair : map')
extend_envi (Environment map' outer)  pair = Environment (pair : map') outer

extend_envi' :: Environment -> [(String, Value)] -> Environment
extend_envi' (Global map') pairs = Global (pairs ++ map')
extend_envi' (Environment map' outer)  pairs = Environment (pairs ++ map') outer

global :: [Stmt] -> Environment
global stmts = Global (constructers_and_atributes_map ++ functions_map)
    where
        functions_map :: Map
        functions_map = map (\(name, f) -> (name, f closure)) (zip function_names functions)
            where
                functions :: [(Environment -> Value)]
                functions = map partial_eval (filter is_func stmts)
                    where
                        is_func :: Stmt -> Bool
                        is_func (Function _ _ _) = True
                        is_func _             = False
                        
                        partial_eval :: Stmt -> (Environment -> Value)
                        partial_eval (Function name parameter body) = Function' name parameter body
                
                function_names :: [String]
                function_names = helper stmts
                    where
                        helper :: [Stmt] -> [String]
                        helper [] = []
                        helper (Function name _ _ : xs) = name : (helper xs)
                        helper (x:xs) = helper xs

                closure :: Environment
                closure = Environment (functions_map) (Global constructers_and_atributes_map)

        constructers_and_atributes_map :: Map
        constructers_and_atributes_map = attribute_functions ++ constructer_functions
            where
                constructer_functions :: Map
                constructer_functions = map (\(Constructer name parameters) -> (name, Function' name parameters (body name) rec_env)) constructers
                    where
                        body :: String -> Expr
                        body name = Lambda [name] (Hack (Name' name))

                        rec_env :: Environment
                        rec_env = Environment constructers_and_atributes_map (Global [])

                attribute_functions :: Map
                attribute_functions = map (\parameter -> (parameter, Function' parameter ["object"] (Call' (Name' "object") [StringExpr parameter]) (Global []))) attr_names
                    where
                        attr_names :: [String]
                        attr_names = concat (map (\(Constructer _ parameters) -> parameters) constructers)

        constructers :: [Constructer]
        constructers = concat (helper stmts)
            where
                helper :: [Stmt] -> [[Constructer]]
                helper [] = []
                helper (ClassDeclre _ constructers' _: xs) = constructers' : (helper xs)
                helper (x:xs) = helper xs  