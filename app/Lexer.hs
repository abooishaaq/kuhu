module Lexer where

import Data.Bool (Bool)
import Text.Parsec (char, letter, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [";", "(", ")", "{", "}", "+", "-", "*", "/", "%", "<<", ">>", "++", "--", "&", "|", ",", ".", "=", "<", "and", "not", "or"]
    names = ["else", "false", "fn", "if", "let", "true"]
    style =
        emptyDef
            { Tok.commentLine = "#"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            , Tok.identStart = letter <|> char '_'
            }

natural :: Parser Integer
natural = Tok.natural lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep1 lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep1 lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
