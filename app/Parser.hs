module Parser where

import Control.Applicative (Alternative (some), optional)
import Data.Functor ((<&>))
import Text.Parsec (ParseError, char, digit, eof, many, many1, manyTill, option, parse, sepBy, space, spaces, string, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (hexadecimal))
import qualified Text.Parsec.Token as Tok
import Text.Read (readMaybe)
import Prelude hiding (seq)

import Lexer
import Syntax
import Type

prefix s c = Ex.Prefix (reservedOp s >> return c)

binary s c f = Ex.Infix (reservedOp s >> return (c f))

table =
    [
        [ binary "." Access () Ex.AssocLeft
        ]
    ,
        [ prefix "-" Negate
        ]
    ,
        [ prefix "++" Incr
        , prefix "--" Decr
        ]
    ,
        [ binary "&" BitOp Andb Ex.AssocLeft
        , binary "|" BitOp Orb Ex.AssocLeft
        , binary "^" BitOp Xor Ex.AssocLeft
        , binary ">>" BitOp Shr Ex.AssocLeft
        , binary "<<" BitOp Shl Ex.AssocLeft
        ]
    ,
        [ binary "%" ArithOp Remainder Ex.AssocLeft
        , binary "*" ArithOp Times Ex.AssocLeft
        , binary "/" ArithOp Divide Ex.AssocLeft
        ]
    ,
        [ binary "+" ArithOp Plus Ex.AssocLeft
        , binary "-" ArithOp Minus Ex.AssocLeft
        ]
    ,
        [ binary "<" CmpOp Less Ex.AssocLeft
        , binary ">" CmpOp More Ex.AssocLeft
        ]
    ,
        [ binary "==" CmpOp Equal Ex.AssocLeft
        , binary "!=" CmpOp Unequal Ex.AssocLeft
        ]
    ,
        [ prefix "not" Not
        ]
    ,
        [ binary "and" BoolOp And Ex.AssocLeft
        ]
    ,
        [ binary "or" BoolOp Or Ex.AssocLeft
        ]
    ]

int :: Parser Expr
int = do
    Int . fromInteger <$> natural

hex :: Parser Expr
hex = do
    char '0'
    Int . fromInteger <$> hexadecimal lexer

number :: Parser String
number = many1 digit

float :: Parser Expr
float = do
    fl <- parser
    case readMaybe fl of
        Just f -> return (Float f)
        Nothing -> fail "Invalid float"
  where
    parser = do
        a <- number
        char '.'
        more <- optional number
        more2 <- optional (char 'e' >> number)
        case more of
            Just b ->
                case more2 of
                    Just c -> return (a ++ "." ++ b ++ "e" ++ c)
                    Nothing -> return (a ++ "." ++ b)
            Nothing -> return (a ++ ".0")

bool :: Parser Expr
bool =
    (reserved "true" >> return (Bool True))
        <|> (reserved "false" >> return (Bool False))

typee :: Parser Type
typee =
    (reserved "int" >> return typeInt)
        <|> (reserved "float" >> return typeFloat)
        <|> (reserved "bool" >> return typeBool)
        <|> (reserved "unit" >> return typeUnit)
        <|> (identifier <&> TStruct1)
        <|> arrayty

arrayty :: Parser Type
arrayty = do
    reservedOp "["
    ty <- typee
    reservedOp "]"
    return (TArray ty)

variable :: Parser Expr
variable = do
    v <- identifier
    let index = do
            reservedOp "["
            i <- expr
            reservedOp "]"
            return i
    indices <- many index
    case indices of
        [] -> return (Var v)
        _ -> return $ foldl Index (Var v) indices

field :: Parser (String, Expr)
field = do
    name <- optional $ identifier <* reservedOp ":"
    val <- expr
    case name of
        Just n -> return (n, val)
        Nothing ->
            case val of
                Var n -> return (n, val)
                _ -> fail "Invalid field"

structex :: Parser Expr
structex = do
    name <- identifier
    reservedOp "{"
    fields <- field `sepBy` reservedOp ","
    reservedOp "}"
    return (StructEx name fields)

nonapp :: Parser Expr
nonapp = do
    parens expr
        <|> try bool
        <|> try float
        <|> try hex
        <|> try int
        <|> variable

app :: Parser Expr
app = do
    f <- term
    args <- parens (expr `sepBy` (spaces >> reservedOp ","))
    return (App f args)

array :: Parser Expr
array = do
    reservedOp "["
    row <- expr `sepBy` spaces
    reservedOp "]"
    return (Array row)

arraybuilder :: Parser Expr
arraybuilder = do
    reservedOp "["
    def <- expr
    reservedOp ";"
    len <- expr
    reservedOp "]"
    return (ArrayBuilder def len)

term = Ex.buildExpressionParser table nonapp

aexp :: Parser Expr
aexp =
    try app <|> term

range :: Parser Expr
range = do
    ex1 <- int
    reservedOp ".."
    Range ex1 <$> aexp

assign :: Parser Expr
assign = do
    v <- variable
    reservedOp "="
    Assign v <$> expr

expr :: Parser Expr
expr =
    try structex
        <|> try range
        <|> try arraybuilder
        <|> try assign
        <|> try array
        <|> aexp

ifthen :: Parser Stmt
ifthen = do
    reserved "if"
    ex1 <- expr
    reservedOp "{"
    st1 <- many stmt
    reservedOp "}"
    els_ <- optional $ reserved "else" >> reservedOp "{"
    case els_ of
        Just _ -> do
            reserved "else"
            st2 <- many stmt
            reservedOp "}"
            return (If ex1 st1 st2)
        Nothing -> return (If ex1 st1 [])

forst :: Parser Stmt
forst = do
    reserved "for"
    v <- identifier
    reserved "in"
    ex <- expr
    reservedOp "{"
    st <- many stmt
    reservedOp "}"
    return (For v ex st)

args :: Parser [(String, Type)]
args = do
    arg `sepBy` (spaces >> reservedOp ",")
  where
    arg = do
        v <- identifier
        reservedOp ":"
        ty <- typee
        return (v, ty)

exprst :: Parser Stmt
exprst = Expr <$> expr

def :: Parser Stmt
def = do
    reserved "let"
    v <- identifier
    reservedOp "="
    Def v <$> expr

returnst :: Parser Stmt
returnst = do
    reserved "return"
    Return <$> expr

stmt :: Parser Stmt
stmt =
    try def
        <|> try forst
        <|> try ifthen
        <|> try returnst
        <|> exprst

fundef :: Parser Fun
fundef = do
    reserved "fn"
    v <- identifier
    ar <- parens args
    reservedOp "->"
    ty <- typee
    reservedOp "{"
    body <- many stmt
    reservedOp "}"
    return (Fun v ar ty body)

funtop :: Parser TopLevel
funtop = TopLevelFun <$> fundef

struct :: Parser TopLevel
struct = do
    reserved "struct"
    v <- identifier
    reservedOp "{"
    ar <- args
    reservedOp "}"
    return (TopLevelStruct (Struct v ar))

impl :: Parser TopLevel
impl = do
    reserved "impl"
    v <- identifier
    reservedOp "{"
    body <- many fundef
    reservedOp "}"
    return (TopLevelImpl (Impl v body))

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [TopLevel]
toplevel = many1 (try struct <|> try impl <|> funtop)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError Program
parseToplevel = parse (contents toplevel) "<stdin>"
