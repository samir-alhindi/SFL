module RuntimeData where

import AST
import Text.Parsec
import Text.Printf
import Data.List (isSuffixOf)

data Value = 
      Number' {get_num :: Double}
    | Boolean' {get_bool :: Bool}
    | String' {get_str :: String}
    | List' [Value]
    | Lambda' [String] Expr Environment
    | Function' String [String] Expr Environment
    | Constructer' String [String]
    | Getter String
    | Object String Map

instance Show Value where
    show (Number' n)             = if ".0" `isSuffixOf` (show n) then show (floor n :: Integer) else show n
    show (Boolean' b  )          = show b
    show (String' s)             = show s
    show (Lambda' _ _ _ )        = "lambda"
    show (Function' name _ _ _ ) = "function " ++ name
    show (Constructer' name _)   = "constructer " ++ name
    show (Getter name)           = "getter " ++ name
    show (List' elements)        = show elements
    show (Object name obj_map)   = name ++ " object {\n" ++ (concat (map (\(key,value) -> "\t" ++ key ++ " : " ++ (show value) ++ "\n") obj_map)) ++ "}"

instance Eq Value where
    (Number' n1)  == (Number' n2)                      = n1 == n2
    (Boolean' b1) == (Boolean' b2)                     = b1 == b2
    (String' s1)  == (String' s2)                      = s1 == s2
    (List' l1)    == (List' l2)                        = l1 == l2
    (Object name1 obj_map1) == (Object name2 obj_map2) = (name1 == name2) && (obj_map1 == obj_map2)
    _ == _                                             = False
    

data Error' = Error' String SourcePos

instance Show Error' where
    show (Error' err_log pos) =
        printf "Error at position (%d,%d): %s" (sourceLine pos) (sourceColumn pos) err_log


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

extend_envi :: Environment -> (String, Value) -> Environment
extend_envi (Global map')             pair = Global (pair : map')
extend_envi (Environment map' outer)  pair = Environment (pair : map') outer

extend_envi' :: Environment -> [(String, Value)] -> Environment
extend_envi' (Global map') pairs = Global (pairs ++ map')
extend_envi' (Environment map' outer)  pairs = Environment (pairs ++ map') outer

global :: [TopLevel] -> Environment
global top_level = Global (constructers_and_getters_map ++ functions_map ++ enum_map)
    where
        functions_map :: Map
        functions_map = map (\(name, f) -> (name, f (global top_level))) (almost_functions)
            where
                almost_functions :: [(String, (Environment -> Value))]
                almost_functions = map partial_eval (declerations top_level)
                    where
                        declerations :: [TopLevel] -> [Function]
                        declerations (TL_Function f : rest) = f : (declerations rest)
                        declerations []             = []
                        declerations (_:rest) = declerations rest
                        
                        partial_eval :: Function -> (String, (Environment -> Value))
                        partial_eval (Function name parameter body) = (name, Function' name parameter body)
                
        constructers_and_getters_map :: Map
        constructers_and_getters_map = getters_map ++ constructers_map
            where
                constructers_map :: Map
                constructers_map = map (\(Constructer name parameters) -> (name, Constructer' name parameters)) constructers

                getters_map :: Map
                getters_map = map (\parameter -> (parameter, Getter parameter)) attr_names
                    where
                        attr_names :: [String]
                        attr_names = concat (map (\(Constructer _ parameters) -> parameters) constructers)

                constructers :: [Constructer]
                constructers = concat (helper top_level)
                    where
                        helper :: [TopLevel] -> [[Constructer]]
                        helper [] = []
                        helper (TL_Class (Class _ constructers' _) : rest) = constructers' : (helper rest)
                        helper (_:rest) = helper rest

        enum_map :: Map
        enum_map = zip (names top_level) (map Number' (map (fromIntegral) ([0..999] :: [Integer])))
                where
                    names :: [TopLevel] -> [String]
                    names [] = []
                    names (TL_Enumeration (Enumeration _ e _) : rest) = e ++ (names rest)
                    names (_:rest) = names rest