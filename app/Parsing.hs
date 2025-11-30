module Parsing where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity
import AST

program :: Parser [Declaration]
program = (many1 decleration) <* eof <?> "program"

decleration :: Parser Declaration
decleration =
    try (TL_Stmt <$> statement) <|>
    try (TL_Function <$> function) <|>
    try (TL_Class <$> class') <|>
    try (TL_Enumeration <$> enum) <|>
    try (TL_Import <$> import')
    
statement :: Parser Stmt
statement =
    try print'
    <?> "statement"

class' :: Parser Class
class' = do
    m_reserved "data"
    pos <- getPosition
    name <- m_identifier
    m_reservedOp "="
    constructers <- constructer `sepBy1` (m_reservedOp "|")
    _ <- m_semi
    return (Class name constructers pos)
    where
        constructer :: Parser Constructer
        constructer = do
            name <- m_identifier
            parameters <- many1 m_identifier
            return (Constructer name parameters)

function :: Parser Function
function = do
    name <- m_identifier
    parameters <- many m_identifier
    m_reservedOp "="
    body <- expression
    _ <- m_semi
    return (Function name parameters body)

enum :: Parser Enumeration
enum = do
    m_reserved "enum"
    name <- m_identifier
    pos <- getPosition
    m_reservedOp "="
    enumerations <- m_identifier `sepBy1` (m_reservedOp "|")
    _ <- m_semi
    return (Enumeration name enumerations pos)

import' :: Parser Import
import' = do
    m_reserved "import"
    path <- m_stringLiteral
    return (Import path)


print' :: Parser Stmt
print' = do
    m_reserved "print"
    expre <- expression
    _ <- m_semi
    return (Print expre) <?> "print"

def :: LanguageDef ()
def = emptyDef {
    opStart = oneOf "+-*/><=!:#%",
    opLetter = oneOf "<>=+",
    reservedOpNames = ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=", "and", "or", "not", "=", "><", "\\", "->", "!", "#", ":", "%", "|", "++", ">>="],
    reservedNames  = ["true", "false", "and", "or", "not", "if", "then", "else", "let", "in", "match", "case", "with", "data", "enum", "import"],
    commentLine = "--",
    commentStart = "{-",
    commentEnd = "-}"
}

m_naturalOrFloat :: Parser (Either Integer Double)
m_parens         :: Parser a -> Parser a
m_braces         :: Parser a -> Parser a
m_brackets       :: Parser a -> Parser a
m_reservedOp     :: String -> Parser ()
m_reserved       :: String -> Parser ()
m_identifier     :: Parser String
m_whiteSpace     :: Parser ()
m_semi           :: Parser String
m_comma          :: Parser String
m_stringLiteral  :: Parser String
TokenParser { 
    naturalOrFloat  = m_naturalOrFloat,
    parens = m_parens,
    braces = m_braces,
    brackets = m_brackets,
    reservedOp = m_reservedOp,
    reserved  = m_reserved,
    identifier = m_identifier,
    whiteSpace = m_whiteSpace,
    semi       = m_semi,
    comma      = m_comma, 
    stringLiteral  = m_stringLiteral
} = makeTokenParser def

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term = try call <|> atom

atom :: Parser Expr
atom = m_parens expression
    <|> try list
    <|> try number
    <|> try ternary
    <|> try (StringExpr <$> m_stringLiteral)
    <|> try boolean
    <|> try identifier'
    <|> try lambda
    <|> try match
    <|> try let_expr

table :: [[Operator String () Identity Expr]]
table = [
    [Prefix (m_reservedOp "-"   >> getPosition >>= \ pos -> return ((Unary pos Negation)))          ],

    [Prefix (m_reservedOp "!"     >> getPosition >>= \ pos -> return (Unary pos Head)),
     Prefix (m_reservedOp "#"     >> getPosition >>= \ pos -> return (Unary pos Tail))],

    [Infix  (m_reservedOp "*"   >> getPosition >>= \ pos -> return (Binary pos Multiply)) AssocLeft,
     Infix  (m_reservedOp "/"   >> getPosition >>= \ pos -> return (Binary pos Divide))   AssocLeft,
     Infix  (m_reservedOp "%"   >> getPosition >>= \ pos -> return (Binary pos Mod))   AssocLeft],

    [Infix  (m_reservedOp "+"   >> getPosition >>= \ pos -> return (Binary pos Plus))     AssocLeft,
     Infix  (m_reservedOp "-"   >> getPosition >>= \ pos -> return (Binary pos Minus))    AssocLeft],

    [Infix (m_reservedOp ">"    >> getPosition >>= \ pos -> return (Binary pos Greater)) AssocNone,
     Infix (m_reservedOp "<"    >> getPosition >>= \ pos -> return (Binary pos Less))    AssocNone,
     Infix (m_reservedOp ">="   >> getPosition >>= \ pos -> return (Binary pos GreaterEqual))   AssocNone,
     Infix (m_reservedOp "<="   >> getPosition >>= \ pos -> return (Binary pos LessEqual))   AssocNone],

    [Infix (m_reservedOp "=="   >> getPosition >>= \ pos -> return (Binary pos DoubleEquals)) AssocLeft,
     Infix (m_reservedOp "!="   >> getPosition >>= \ pos -> return (Binary pos NotEquals))   AssocLeft],

    [Infix (m_reservedOp ":"    >> getPosition >>= \ pos -> return (Binary pos Cons)) AssocLeft,
     Infix (m_reservedOp "++"   >> getPosition >>= \ pos -> return (Binary pos Concat)) AssocLeft],

    [Prefix (m_reservedOp "not" >> getPosition >>= \ pos -> return (Unary pos Not))                              ],
    [Infix  (m_reservedOp "and" >> getPosition >>= \ pos -> return (Binary pos And)) AssocLeft                    ],
    [Infix  (m_reservedOp "or"  >> getPosition >>= \ pos -> return (Binary pos Or)) AssocLeft                      ],
    
    [Infix (m_reservedOp "><"   >> getPosition >>= \ pos -> return (Binary pos Curry)) AssocLeft,
     Infix (m_reservedOp ">>="  >> getPosition >>= \ pos -> return (Binary pos Bind)) AssocLeft]]    

number :: Parser Expr
number = f <$> m_naturalOrFloat
    where
        f :: Either Integer Double -> Expr
        f (Left i) = Number (fromIntegral i)
        f (Right d)  = Number d

identifier' :: Parser Expr
identifier' = do
    name <- m_identifier
    pos <- getPosition
    return (Name pos name)

boolean :: Parser Expr
boolean = (m_reserved "true"   >> return (Boolean True ))
    <|> (m_reserved "false" >> return (Boolean False))

let_expr :: Parser Expr
let_expr = do
    m_reserved "let"
    name <- m_identifier
    m_reservedOp "="
    init' <- expression
    m_reserved "in"
    result <- expression
    return (LetExpr name init' result)


lambda :: Parser Expr
lambda = do
    m_reservedOp "\\"
    parameters <- many1 m_identifier
    m_reservedOp "->"
    body <- expression
    return (Lambda parameters body)

call :: Parser Expr
call = do
    callee <- valid_callee <|> (m_parens valid_callee)
    pos <- getPosition
    args <- many1 atom
    return (Call pos callee args)
        where
            valid_callee :: Parser Expr
            valid_callee = identifier' <|> lambda <|> (m_parens expression)

ternary :: Parser Expr
ternary = do
    m_reserved "if"
    pos <- getPosition
    condition <- expression
    m_reserved "then"
    then_branch <- expression
    m_reserved "else"
    else_branch <- expression
    return (Ternary pos condition then_branch else_branch)

list :: Parser Expr
list = do
    elements' <- m_brackets (elements <|> empty_list)
    return (List elements')

    where
        elements :: Parser [Expr]
        elements = do
            first <- expression
            rest <- many (m_comma >> expression)
            return (first : rest)
        
        empty_list :: Parser [Expr]
        empty_list = return []

match :: Parser Expr
match = do
    m_reserved "match"
    pos <- getPosition
    expr <- expression
    m_reserved "with"
    patterns <- many1 pattern'
    m_reserved "else"
    m_reservedOp "->"
    wild_card <- expression
    return (Match pos expr patterns wild_card)
    where
        pattern' :: Parser (Pattern, Expr)
        pattern' = do
            m_reserved "case"
            pattern'' <- patterns
            m_reservedOp "->"
            result <- expression
            return (pattern'', result)
            where
                
                patterns :: Parser Pattern
                patterns =
                    try (m_parens destructer <|> destructer) <|>
                    try (m_parens list_pattern <|> list_pattern) <|>
                    (ExprPattern <$> expression)

                destructer :: Parser Pattern
                destructer = do
                    constructer_name <- m_identifier
                    attributes <- many1 m_identifier
                    DestructerPattern <$> (return (Destructer constructer_name attributes))
                
                list_pattern :: Parser Pattern
                list_pattern = do
                    x <- m_identifier
                    m_reservedOp ":"
                    xs <- m_identifier
                    ListPattern <$> (return (x, xs))


my_parse :: String -> Either ParseError [Declaration]
my_parse source = parse (m_whiteSpace >> program) "" source