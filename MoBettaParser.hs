module MoBettaParser where

-- Parser for MoBetta.

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Data.Void

import MoBettaAST

-- Simplest use of Parsec is all we need.

type Parser = Parsec Void String


programParser = do
  spaceConsumer
  sepEndBy1 statementParser semicolon <?>  "program"


statementParser = choice
  [   skipStmt
    , printStmt
    , messageStmt
    , readStmt
    , ifStmt
    , whileStmt
    , assignmentStmt
    , blockStmt
  ] <?> "statement"
  where
    skipStmt = lexeme (string "skip") >> return Skip
    printStmt = do
      lexeme (string "print")
      e <- aExpr
      return (Print e)
    readStmt = do
      lexeme (string "read")
      i <- identifier
      return (Read i)
    messageStmt = do
      lexeme (string "message")
      s <- lexeme stringLiteral
      return (Msg s)
    ifStmt = do
      lexeme (string "if")
      b <- bExpr
      lexeme (string "then")
      t <- statementParser
      lexeme (string "else")
      e <- statementParser
      return (If b t e)
    whileStmt = do
      lexeme (string "while")
      b <- bExpr
      lexeme (string "do")
      e <- statementParser
      return (While b e)
    assignmentStmt = do
      v <- identifier
      lexeme (char '=')
      e <- aExpr
      return (Assign v e)
    blockStmt = do
      stmts <- between lbrace rbrace programParser
      return (Block stmts)

aExpr = makeExprParser aFactor aOpTable <?> "arithmetic expression"

-- parenthesized expressions are missing
aFactor = choice [ intConst
                , identifierExpr
                , between lparen rparen aExpr
                ] <?> "arithmetic factor"

aOpTable = [ [ prefix  "-"  (AUn Neg)
            , prefix  "+" id ] -- including a prefix + sign
          , [ binary  "*"  (ABin Mul)
            , binary  "/"  (ABin Div)
            , binary  "%"  (ABin Mod)]
          , [ binary  "+"  (ABin Add)
            , binary  "-"  (ABin Sub)  ] ]

bExpr = makeExprParser bFactor bOpTable <?> "Boolean expression"

bFactor = choice [ comparison
                 , between lparen rparen bExpr
                 ] <?> "boolean factor"

bOpTable = [ [ prefix  "not"  (BUn Not)]
           , [ binary  "and"  (BBin And)
             , binary  "or"  (BBin Or)] ]

comparison = do
    e1 <- aExpr
    c  <- comparator
    e2 <- aExpr
    return (Reln c e1 e2)

comparator = choice compTable <?> "comparator"

compTable = [
    atomic "<"  Less
  , atomic "<=" LessEqual
  , atomic ">"  Greater
  , atomic ">=" GreaterEqual
  , atomic "==" Equal
  , atomic "!=" NEqual
  ]

-- These help declare operators
binary  opName f = InfixL (atomic opName f) -- (f <$ lexeme (string opName))
prefix  opName f = Prefix (atomic opName f)
atomic  opName f = f <$ lexeme (string opName)


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that consumes space after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lparen = lexeme (char '(')
rparen = lexeme (char ')')
semicolon = lexeme (char ';')
lbrace = lexeme (char '{')
rbrace = lexeme (char '}')


identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

identifierExpr = Var <$> identifier

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

intConst :: Parser AExpr
intConst = fmap IntConst intConst'
  where
    intConst' = (lexeme . try) ic
    ic = do
          x <- L.decimal -- parse a literal
          notFollowedBy letterChar -- fail if followed by a letter
          return x -- return the  result if we haven't failed

tryit p = parse p "(--)"

mbparse = parse programParser
