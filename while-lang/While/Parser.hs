{-# LANGUAGE InstanceSigs #-}
module While.Parser where

import Prelude hiding (not)

import While.Language

import Data.Text (Text, pack)
import Data.Void

import Control.Monad.Extra

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Parser type
type Parser = Parsec
    Void -- An error is a runtime error
    Text -- Accepts Text as input

parseWhileProgram :: String -> String -> Either (ParseErrorBundle Text Void) Stmt
parseWhileProgram label content = parse parseTerm label (pack content)

-- Input functions
parseString :: Parser a -> String -> Either (ParseErrorBundle Text Void) a
parseString p = parseText p . pack

parseText :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseText p = parse p ""

-- Arithmetic expressions parser
instance Parsable AExp where
  leaves :: [Parser AExp]
  leaves =
    [ parseNat
    , parseInc
    , parseDec
    , parsePrefixInc
    , parsePrefixDec
    , parseVar
    , parseNeg
    , betweenRound parseTerm
    ]
  binops :: [Parser (BinOp AExp)]
  binops =
    [ operMul <$ symbol "*"
    , operSum <$ symbol "+"
    , operSub <$ symbol "-"
    , operDiv <$ symbol "/"
    ]

parseNat :: Parser AExp
parseNat = Nat <$> lexeme L.decimal

parseVar :: Parser AExp
parseVar = Var <$> lexeme parseVariable

parseNeg :: Parser AExp
parseNeg = do
    symbol "-"
    Neg <$> parseLeaf

parseInc :: Parser AExp
parseInc = lexeme $ do
    var <- parseVariable -- no spaces
    symbol "++"
    -- C.space1 -- at least one space per favore
    return (Inc var)

parseDec :: Parser AExp
parseDec = lexeme $ do
    var <- parseVariable -- no spaces
    symbol "--"
    -- C.space1 -- at least one space per favore
    return (Dec var)

parsePrefixInc :: Parser AExp
parsePrefixInc = lexeme $ do
    symbol "++"
    PrefixInc <$> parseVariable


parsePrefixDec :: Parser AExp
parsePrefixDec = lexeme $ do
    symbol "--"
    PrefixDec <$> parseVariable

instance Parsable BExp where
  leaves :: [Parser BExp]
  leaves = 
    [ parseLit
    , parseNot
    , parseCmp
    , betweenRound parseTerm
    ]
  binops :: [Parser (BinOp BExp)]
  binops = 
    [ operAnd <$ keyword "and"
    , operOr  <$ keyword "or"
    ]

parseLit :: Parser BExp
parseLit = choice
    [ Lit True  <$ keyword "true"
    , Lit False <$ keyword "false"
    ]

parseNot :: Parser BExp
parseNot = do
    keyword "not"
    not <$> parseLeaf

parseCmp :: Parser BExp
parseCmp = do
    e1 <- parseTerm
    cmp <- parseComparisonOperator
    cmp e1 <$> parseTerm

parseComparisonOperator :: Parser (AExp -> AExp -> BExp)
parseComparisonOperator = choice
    [ Eq          <$ symbol "="
    , Low         <$ symbol "<"
    , Neq         <$ symbol "!="
    , GEq         <$ symbol ">="
    , lowerEqual  <$ symbol "<="
    , greaterThan <$ symbol ">"
    ]

instance Parsable Stmt where
  leaves :: [Parser Stmt]
  leaves = 
    [ parseAssign
    , parseSkip
    , parseBrnc
    , parseLoop
    , betweenCurly parseTerm
    ]
  binops :: [Parser (BinOp Stmt)]
  binops = [ operCons <$ symbol ";"]

parseAssign :: Parser Stmt
parseAssign = do
    x <- parseVariable
    symbol ":="
    Assg x <$> parseTerm

parseSkip :: Parser Stmt
parseSkip = Skip <$ keyword "skip"

parseBrnc :: Parser Stmt
parseBrnc = do
    keyword "if"
    b <- parseTerm
    keyword "then"
    s1 <- parseLeaf
    keyword "else"
    Brnc b s1 <$> parseLeaf

parseLoop :: Parser Stmt
parseLoop = do
    keyword "while"
    b <- parseTerm
    keyword "do"
    Loop b <$> parseLeaf 

-- Main Parsable typeclass
class Parsable a where
    leaves :: [Parser a]
    binops :: [Parser (BinOp a)]
    parseLeaf :: Parser a
    parseLeaf = choice (try <$> leaves)
    parseTerm :: Parser a
    parseTerm = parseTerm' 0

    {- All arithmetic binary operators in while are left 
        associative, and there are only two levels of 
        precedence.
        Here for simplicity the algorithm is presented as 
    -}
    parseTerm' :: Int -> Parser a
    parseTerm' depth = do
        -- expects at least one leaf
        term <- parseLeaf
        loopM collectBinaryOperators term
        where
            collectBinaryOperators term1 = do
                -- try doesn't consume anything when the inner parser fails
                op <- try $ do
                    -- parse an arithmetic binary operator
                    -- but accept it only if it has at least
                    -- the precedence as the depth
                    op <- choice binops
                    if prio op >= depth
                        then return op
                        else empty
                -- if assoc is right to left 
                -- then stop or parse only with higher precedence
                -- else continue recursively
                let newDepth = prio op + case assc op of
                        LeftToRight -> 1
                        RightToLeft -> 0
                -- if the operator is accepted then parse
                -- a higher priority expression
                -- and continue iterating
                Left . term op term1 <$> parseTerm' newDepth
                <|>
                -- if the operator is not accepted (or it 
                -- wasn't an operator at all) the AExp is
                -- over
                return (Right term1)

-- Language utilities

-- TODO: For now variables are lower characters only
parseVariable :: Parser String
parseVariable = lexeme $ some C.lowerChar

-- Utilities

-- This space consumer consumes all spaces, expecting none
sc :: Parser ()
sc = L.space C.space1 -- consume as many spaces possible
    empty -- expect no line comment
    empty -- expect no multi-line comment

{- Lexeme: behaves as the input parser would,
    and then it consumes all white spaces after
-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

{- Symbol: parses the string given as argument
    and then consumes all trailing white spaces.
    This will be used only with symbols
-}
symbol :: Text -> Parser Text
symbol = L.symbol sc

{- Keyword: parses the string given as argument
    expecting a space after it.
    This will be used by the keywords of the 
    language.
-}
keyword :: Text -> Parser Text
keyword kwd = lexeme (C.string kwd <* notFollowedBy C.alphaNumChar)

{- Parses a parser between round parenthesis
-}
betweenRound :: Parser a -> Parser a
betweenRound = between (symbol "(") (symbol ")")

{- Parses a parser between curly parenthesis
-}
betweenCurly :: Parser a -> Parser a
betweenCurly = between (symbol "{") (symbol "}")


