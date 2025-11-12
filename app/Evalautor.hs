module Evalautor where

import AST
import RuntimeData
import Text.Parsec
import Data.Fixed (mod')

exec_program :: [TopLevel] -> Either Error' (IO ())
exec_program program = helper program (global program)
        where
            helper :: [TopLevel] -> Environment -> Either Error' (IO ())
            helper [] _ = Right (return ())
            helper (TL_Stmt stmt:rest) envi = do
                (envi', io) <- exec stmt envi
                io'         <- helper rest envi'
                return (io >> io')
            helper (_:rest) envi = helper rest envi

exec ::  Stmt -> Environment -> Either Error' (Environment, IO())
exec (Print expr) envi = do
    result <- eval expr envi
    return (envi, print result)

exec (If pos condition then_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition'
        then exec then_branch envi
        else Right (envi, return ())

exec (IfElse pos condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition'
        then exec then_branch envi
        else exec else_branch envi

exec (Block stmts) envi = do
    let envi' = Environment [] envi
    (_, io) <- exec_block stmts envi'
    Right (envi, io)
    where
        exec_block :: [Stmt] -> Environment -> Either Error' (Environment, IO())
        exec_block [] envi'' = Right (envi'', return ())
        exec_block (stmt:rest) envi'' = do
            (envi''', io) <- exec stmt envi''
            (_, io')    <- exec_block rest envi'''
            Right (envi'', (io >> io'))

eval :: Expr -> Environment -> Either Error' Value
eval (Ternary pos condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition' then eval then_branch envi else eval else_branch envi

eval (StringExpr str) _ = Right (String' str)

eval (Name pos name) envi = (find envi name pos) >>= eval_name
eval (Name' name) envi = (find' envi name) >>= eval_name

eval (Number n) _ = Right (Number' n)
eval (Boolean b) _ = Right (Boolean' b)

eval (Binary pos opp e1 e2) envi
    | opp `elem` [Minus, Multiply, Divide, Mod] = binary_number
    | opp `elem` [And, Or]                 = binary_boolean
    | opp == Plus                          = plus
    | opp == Curry                         = curry'
    | opp == Bind                          = undefined
    | opp == Cons                          = cons
    | opp == Concat                        = concatenate
    | otherwise                            = relational
    where
        binary_number :: Either Error' Value
        binary_number = do
            (n1, n2) <- check_number_opperands
            Right $ Number' $ case opp of
                Minus    -> n1 - n2
                Multiply -> n1 * n2
                Divide   -> n1 / n2
                Mod      -> mod' n1 n2
                _        -> -1 -- This will never run.
        
        binary_boolean :: Either Error' Value
        binary_boolean = do
            (b1, b2) <- check_boolean_opperands
            Right $ Boolean' $ case opp of
                And -> b1 && b2
                Or  -> b1 || b2
                _   -> False -- This will never run.
        
        relational :: Either Error' Value
        relational
            | opp `elem` [DoubleEquals, NotEquals] = do
                    n1 <- eval e1 envi
                    n2 <- eval e2 envi
                    Right $ Boolean' $ case opp of
                        DoubleEquals -> n1 == n2
                        NotEquals    -> n1 /= n2
                        _            -> False -- This will never run.

            | opp `elem` [Greater, Less, GreaterEqual, LessEqual] = do
                (n1, n2) <- check_number_opperands
                Right $ Boolean' $ case opp of
                    Less         -> n1 < n2
                    Greater      -> n1 > n2
                    GreaterEqual -> n1 >= n2
                    LessEqual    -> n1 <= n2
                    _            -> False -- This will never run.

            | otherwise = Left (Error' "This will never run." pos)

        plus :: Either Error' Value
        plus = do
            e1' <- eval e1 envi
            e2' <- eval e2 envi
            case (e1', e2') of
                (Number' n1, Number' n2) -> return (Number' (n1 + n2))
                (String' s1, String' s2) -> return (String' (s1 ++ s2))
                _ -> Left (Error'("cannot add value of types "++(type_of e1')++" and "++(type_of e2')) pos)
    
        curry' :: Either Error' Value
        curry' = do
            f <- eval e1 envi
            x <- eval e2 envi
            case f of
                (Lambda'        parameters body closure) -> helper x (Lambda')        parameters body closure (length parameters)
                (Function' name parameters body closure) -> helper x (Function' name) parameters body closure (length parameters)
                _ -> Left (Error' ("Left '><' opperand must be a callable and not of type " ++ (type_of f)) pos)
 
            where
                helper :: Value
                        -> ([String] -> Expr -> Environment -> Value)
                        -> [String] -> Expr -> Environment -> Int -> Either Error' Value
                helper x callable parameters body closure arity =
                    if arity == 1
                        then eval (Call pos e1 [e2]) envi
                        else
                            let closure' = Environment ((head parameters, x) : []) closure
                            in Right (callable (tail parameters) body closure')

        cons :: Either Error' Value
        cons = do
            head'  <- eval e1 envi
            tails <- eval e2 envi
            case tails of
                (List' elements) -> Right (List' (head' : elements))
                _                -> Left  (Error' ("second ':' opperand must be a list and not of type " ++ type_of tails) pos)

        concatenate :: Either Error' Value
        concatenate = do
            l1 <- eval e1 envi
            l2 <- eval e2 envi
            case (l1, l2) of
                (List' v1, List' v2) -> return (List' (v1 ++ v2))
                _                    -> Left (Error' ("'++' opperands must be lists and not of type "++(type_of l1)++" and "++(type_of l2)) pos)


        check_number_opperands :: Either Error' (Double, Double)
        check_number_opperands = do
            n1 <- eval e1 envi
            n2 <- eval e2 envi
            (n1', n2') <- helper (n1, n2)
            return (n1', n2')
            where
                helper :: (Value, Value) -> Either Error' (Double, Double)
                helper (Number' n1, Number' n2) = Right (n1, n2)
                helper _ = Left (Error' "Both opperands must be numbers." pos)

        check_boolean_opperands :: Either Error' (Bool, Bool)
        check_boolean_opperands = do
            b1 <- eval e1 envi
            b2 <- eval e2 envi
            (b1', b2') <- helper (b1, b2)
            return (b1', b2')
            where
                helper :: (Value, Value) -> Either Error' (Bool, Bool)
                helper (Boolean' b1, Boolean' b2) = Right (b1, b2)
                helper _ = Left (Error' "Both opperands must be booleans." pos)


eval (Unary pos opp e) envi = do
    v <- eval e envi
    case opp of
        Negation ->
            case v of
            Number' n -> Right (Number' (-n))
            _ -> Left (Error' ("'-' opperator must be a number and not of type "++(type_of v)) pos)
        Not ->
            case v of
            Boolean' b -> Right (Boolean' (not b))
            _ -> Left (Error' ("'not' opperand must be a boolean and not of type "++(type_of v)) pos)
        Head ->
            case v of
                (List' list) -> if length list == 0
                    then Left  (Error' "cannot get head of empty list" pos)
                    else Right (head list)
                _ -> Left (Error' ("'!' opperand must be a list and not of type "++(type_of v)) pos)

        Tail ->
            case v of
                List' (_:xs) -> Right (List' xs)
                List' []     -> Left  (Error' ("Cannot get tail of empty list") pos)
                _ -> Left (Error' ("'#' opperand must be a list and not of type "++(type_of v)) pos)

eval (Lambda parameters body) envi = Right (Lambda' parameters body envi )

eval (Call' callee args) envi = eval_call envi callee args (Error'')
eval (Call pos callee args) envi = eval_call envi callee args (`Error'` pos)

eval (LetExpr name init' body) envi = do
    value <- eval init' envi
    let envi' = Environment [(name, value)] envi
    result <- eval body envi'
    return result

eval (List elements) envi = do
    elements' <- sequence (map (`eval` envi) elements)
    return (List' elements')

eval (Match pos expr cases wild_card) envi = (eval expr envi) >>= (\ value -> (check_cases value cases))
    where
        check_cases :: Value -> [(Expr, Expr)] -> Either Error' Value
        check_cases _ [] = eval wild_card envi
        check_cases value@(Lambda' (x:_) _ closure) ((Destructer constructer_name destructer_attributes, branch):rest) =
            if x == constructer_name
                then branch_envi >>= (\envi' -> eval branch envi')
                else check_cases value rest
            where
                branch_envi :: Either Error' Environment
                branch_envi = do
                    attributes <- og_attributes
                    values <- sequence (map (\attr -> eval (Call pos expr [StringExpr attr]) envi) attributes)
                    return (extend_envi' envi (zip destructer_attributes values))
                        where
                            og_attributes :: Either Error' [String]
                            og_attributes =
                                case find closure constructer_name pos of
                                    Left err            -> Left err
                                    Right (Function' _ attributes _ _) -> Right attributes
                                    _ -> Left (Error'' "This shouldn't run, It'll always find the constructer in the lambda's own closure.")

        -- Destructers should never be evalauted:                             
        check_cases value ((Destructer _ _, _) : rest) = check_cases value rest

        check_cases value ((case', branch) : rest) = do
            result <- eval case' envi
            if result == value
                then eval branch envi
                else check_cases value rest

eval (Destructer _ _) _ = Left (Error'' "Destructers shouldn't be evalauted like that.")

eval (Hack expr) envi = case expr of
    (Name' name) -> do
        name' <- eval (Name' name) envi
        result <- eval (Name' ( get_str name')) envi
        return result
    _ -> Left (Error'' "This will never run.")

eval_call :: Environment -> Expr -> [Expr] -> (String -> Error') -> Either Error' Value
eval_call envi callee args error' = do
    callee' <- eval callee envi
    case callee' of
        (Function' _ parameters body closure) -> helper parameters body closure (length parameters)
        (Lambda'     parameters body closure) -> helper parameters body closure (length parameters)
        _                                           -> Left (error' ("Cannot call: " ++ (type_of callee')))
    where
        helper :: [String] -> Expr -> Environment -> Int -> Either Error' Value
        helper parameters body closure arity  = do
            check_arity arity (length args)
            args' <- sequence (map ((flip eval) envi) args)
            let pairs = zip parameters args'
            let envi' = Environment pairs closure
            result <- eval body envi'
            return result

        check_arity :: Int -> Int -> Either Error' ()
        check_arity expected_arity actual_arity = if expected_arity == actual_arity then Right () else Left (error' ("Exptected an arity of " ++ (show expected_arity) ++ " but got " ++ (show actual_arity)))

eval_name :: Value -> Either Error' Value
eval_name value = case value of
                    (Function' _ [] body closure) -> eval body closure
                    _ -> Right value

is_bool :: Environment -> Expr -> SourcePos -> Either Error' Bool
is_bool envi expr pos = case eval expr envi of
    Right (Boolean' b) -> Right b
    Left err -> Left err
    _         -> Left (Error' "condition must be a boolean value." pos)

type_of :: Value -> String
type_of v = case v of
    String' _           -> "string"
    Number' _           -> "number"
    Boolean'   _        -> "boolean"
    Lambda' _ _ _       -> "lambda"
    Function' _ _ _ _   -> "function"
    List' _             -> "list"