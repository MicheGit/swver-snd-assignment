module While.Parser where

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

-- Input functions
parseString :: Parser a -> String -> Either (ParseErrorBundle Text Void) a
parseString p = parseText p . pack 

parseText :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseText p = parse p ""

-- Arithmetic expressions parser
{- All arithmetic binary operators in while are left 
    associative, and there are only two levels of 
    precedence.
    Here for simplicity the algorithm is presented as 
-}
parseAExp :: Parser AExp
parseAExp = parseAExp' 0

parseAExp' :: Int -> Parser AExp
parseAExp' depth = do
    -- expects at least n, x, -e, (e)
    term <- parseAExpLeaf
    loopM collectBinaryOperators term
    where
        collectBinaryOperators :: AExp -> Parser (Either AExp AExp)
        collectBinaryOperators term1 = do
            -- try doesn't consume anything when the inner parser fails
            op <- try $ do
                -- parse an arithmetic binary operator
                -- but accept it only if it has at least
                -- the precedence as the depth
                op <- parseArithmeticBinOp
                if prec op >= depth
                    then return op
                    else empty
            -- since all binary operators are left associative, 
            -- the recursive call parses only operators with higher
            -- precedence
            let newDepth = prec op + 1
            -- if the operator is accepted then parse
            -- a higher priority expression
            -- and continue iterating
            Left . term op term1 <$> parseAExp' newDepth
            <|>
            -- if the operator is not accepted (or it 
            -- wasn't an operator at all) the AExp is
            -- over
            return (Right term1)

parseAExpLeaf :: Parser AExp
parseAExpLeaf = choice
    [ parseNat
    , parseVar
    , parseNeg
    , betweenParenthesis parseAExp]

parseNat :: Parser AExp
parseNat = Nat <$> lexeme L.decimal

parseVar :: Parser AExp
parseVar = Var <$> lexeme parseVariable

parseNeg :: Parser AExp
parseNeg = do
    symbol "-"
    Neg <$> parseAExpLeaf

parseArithmeticBinOp :: Parser BinOp
parseArithmeticBinOp = choice
    [ operMul <$ symbol "*"
    , operSum <$ symbol "+"
    , operSub <$ symbol "-"
    , operDiv <$ symbol "/"
    ]

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
betweenParenthesis :: Parser a -> Parser a
betweenParenthesis = between (symbol "(") (symbol ")")


