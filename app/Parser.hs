module Parser where

import Control.Applicative (Alternative (some), optional)
import Lexer
import Syntax
import Text.Parsec (ParseError, char, digit, eof, many, many1, manyTill, option, parse, sepBy, space, spaces, string, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (hexadecimal))
import qualified Text.Parsec.Token as Tok
import Text.Read (readMaybe)
import Prelude hiding (seq)

prefix s c = Ex.Prefix (reservedOp s >> return c)

binary s c f = Ex.Infix (reservedOp s >> return (c f))

table =
    [
        [ prefix "-" Negate
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

variable :: Parser Expr
variable = do
    Var <$> identifier

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
    f <- identifier
    args <- parens (expr `sepBy` (spaces >> reservedOp ","))
    return (App f args)

array :: Parser Expr
array = do
    reservedOp "["
    row <- expr `sepBy` spaces
    reservedOp "]"
    return (Array row)

term = Ex.buildExpressionParser table nonapp

aexp :: Parser Expr
aexp =
    try app <|> term

range :: Parser Expr
range = do
    ex1 <- int
    reservedOp ".."
    Range ex1 <$> aexp

index :: Parser Expr
index = do
    ex1 <- aexp
    ex2 <- optional $ do
        reservedOp "["
        ex2 <- range <|> aexp
        reservedOp "]"
        return ex2
    case ex2 of
        Just ex2 -> return (Index ex1 ex2)
        Nothing -> return ex1

assign :: Parser Expr
assign = do
    v <- index
    reservedOp "="
    Assign v <$> expr

incr :: Parser Expr
incr = do
    reservedOp "++"
    Incr <$> index

decr :: Parser Expr
decr = do
    reservedOp "--"
    Decr <$> index

expr :: Parser Expr
expr = try assign <|> try incr <|> try decr <|> try range <|> try index <|> array

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

fundef :: Parser Stmt
fundef = do
    reserved "fn"
    v <- identifier
    args <- parens (identifier `sepBy` (spaces >> reservedOp ","))
    reservedOp "{"
    body <- many stmt
    reservedOp "}"
    return (Fun v args body)

exprst :: Parser Stmt
exprst = Expr <$> expr

def :: Parser Stmt
def = do
    reserved "let"
    v <- identifier
    reservedOp "="
    Def v <$> expr

stmt :: Parser Stmt
stmt = try def <|> try forst <|> try ifthen <|> try fundef <|> exprst

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser TopLevel
toplevel = do
    spaces
    (TopLevel <$> many1 stmt) <* eof

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError TopLevel
parseToplevel = parse (contents toplevel) "<stdin>"
