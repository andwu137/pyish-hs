module Language.PyIsh.Parser (
    statement,
    expr,
) where

import Control.Monad (forM_)
import qualified Control.Monad.Combinators.Expr as MC
import Data.Functor (void, ($>), (<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Language.PyIsh.IR.AST as AST
import Language.PyIsh.Utils
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Text.Megaparsec.Debug as P

-- Types
type Parser = P.Parsec Void Text.Text

-- Helpers
hspace :: Parser ()
hspace = space' P.hspace1

space :: Parser ()
space = space' P.space1

space' :: Parser () -> Parser ()
space' p =
    PL.space
        p
        (PL.skipLineComment "#")
        (PL.skipBlockComment "\"\"\"" "\"\"\"")

lexeme :: Parser a -> Parser a
lexeme = PL.lexeme hspace

symbol :: Text.Text -> Parser Text.Text
symbol = PL.symbol hspace

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

withPos :: (P.SourcePos -> a -> b) -> Parser a -> Parser b
withPos f p = do
    pos <- P.getSourcePos
    f pos <$> p

-- Main
test = do
    let tests =
            [ "def f(x: int):\n\
              \    def g(y: list[int]):\n\
              \        \"\"\" comment lol \"\"\"\n\
              \        h = [big, ahh, listlmao] #comment\n\
              \        # comment\n\
              \        h = []\n\
              \        z = x(y + 1 * 5 ** 4 + 3 / 3 / 6 * -3 ** 8)\n\
              \        return z\n\
              \    return g\n"
            , "def f(x: lmao):\n\
              \    h = {big, ahh, set, lmao}\n\
              \    h = {}\n\
              \    if x(y):\n\
              \        return x(y)\n"
            , "def f(x: ski):\n\
              \    h = {dict: mightwork, it: mightnot}\n\
              \    if x(y):\n\
              \        return x(y)\n\
              \    elif y(x):\n\
              \        return y(x) "
            , "def f(x: dict):\n\
              \    h = (faketuple)\n\
              \    h = (real, tuple)\n\
              \    if x(y):\n\
              \        return x(y)\n\
              \    elif y(x):\n\
              \        return y(x)\n\
              \    else:\n\
              \        z = x\n\
              \        return z"
            ]

    forM_ tests $ \t -> do
        putStrLn ">>> Input:"
        putStrLn $ Text.unpack t
        putStrLn "<<< Output:"
        doits (AST.showStatement <$> define) t
        putStrLn "--- End Test\n\n"
        void $ getLine

-- TODO: Uangn: move this
doit p s = case P.parse p "file" s of
    Left e -> putStrLn $ P.errorBundlePretty e
    Right x -> print x

-- TODO: Uangn: move this
doits p s = case P.parse p "file" s of
    Left e -> putStrLn $ P.errorBundlePretty e
    Right x -> putStrLn $ Text.unpack x

typeInfo :: Parser ()
typeInfo =
    void $ P.optional (symbol ":" <* piece)
  where
    piece =
        P.some P.letterChar
            <* P.optional (P.between (symbol "[") (symbol "]") piece)

statement :: Parser (AST.Statement P.SourcePos)
statement =
    P.choice
        [ define
        , ifstmt
        , return'
        , yield
        , assign
        , withPos AST.SExpr expr
        ]

return' :: Parser (AST.Statement P.SourcePos)
return' =
    P.label "return" $
        withPos AST.SReturn $
            P.try (symbol "return") *> expr

yield :: Parser (AST.Statement P.SourcePos)
yield =
    P.label "yield" $
        withPos AST.SYield $
            P.try (symbol "yield") *> expr

assign :: Parser (AST.Statement P.SourcePos)
assign =
    P.label "assign" $
        withPos (uncurry . AST.SAssign) $
            (,)
                <$> P.try (typedIdent' <* symbol "=")
                <*> expr

ifstmt :: Parser (AST.Statement P.SourcePos)
ifstmt =
    withPos (\p (i, ei, e) -> AST.SIf p i ei e) $ do
        let topIf = (,) <$> (P.try (symbol "if") *> expr <* symbol ":")
            topElif = (,) <$> (symbol "elif" *> expr <* symbol ":")
            topElse = (symbol "else" *> symbol ":") $> id
        if' <- block topIf statement
        elif' <- P.many $ block topElif statement
        else' <- P.optional $ block topElse statement
        pure (if', elif', else')

define :: Parser (AST.Statement P.SourcePos)
define =
    withPos (\p (f, n, b) -> AST.SDefine p f n b) $ do
        let top =
                (,,)
                    <$> (P.try (symbol "def") *> ident')
                    <*> ( parens (typedIdent' `P.sepBy` symbol ",")
                            <* symbol ":"
                        )
        block top statement

block :: Parser ([a] -> b) -> Parser a -> Parser b
block h p =
    PL.indentBlock space block'
  where
    block' =
        h <&> \f ->
            PL.IndentSome Nothing (pure . f) p

-- Expr
expr :: Parser (AST.Expr P.SourcePos)
expr =
    MC.makeExprParser term opTable
  where
    term =
        P.choice
            [ collection
            , parens expr
            , apply
            , withPos AST.EAtom atom
            ]

    sym f n = withPos f $ symbol n
    binaryl name f = MC.InfixL (sym f name)
    binaryr name f = MC.InfixR (sym f name)
    prefix name f = MC.Prefix (sym f name)
    postfix name f = MC.Postfix (sym f name)

    opTable =
        [
            [ prefix "-" $ \p _ -> AST.EPrimUnaryOp p AST.Negate
            , prefix "+" $ \_ _ -> id
            ]
        ,
            [ binaryr "**" $ \p _ -> AST.EPrimBinOp p AST.Exp
            ]
        ,
            [ binaryl "*" $ \p _ -> AST.EPrimBinOp p AST.Mul
            , binaryl "/" $ \p _ -> AST.EPrimBinOp p AST.Div
            ]
        ,
            [ binaryl "+" $ \p _ -> AST.EPrimBinOp p AST.Add
            , binaryl "-" $ \p _ -> AST.EPrimBinOp p AST.Sub
            ]
        ]

-- function
apply :: Parser (AST.Expr P.SourcePos)
apply = withPos (uncurry . AST.EApply) apply'

apply' :: Parser (AST.Identifier, [AST.Expr P.SourcePos])
apply' =
    P.try $ -- NOTE: Uangn: i have no idea why i need this
        (,)
            <$> P.try ident'
            <*> ( P.between (P.try $ symbol "(") (symbol ")") $
                    (expr `P.sepBy` symbol ",")
                )

-- Atom
atom :: Parser AST.Atom
atom = P.choice [ident, bool, string, P.try float, int]

bool :: Parser AST.Atom
bool =
    lexeme $
        AST.ABool
            <$> (("True" $> True) <|> ("False" $> False))

int :: Parser AST.Atom
int = lexeme $ AST.AInt <$> PL.signed P.empty PL.decimal

float :: Parser AST.Atom
float = lexeme $ AST.AFloat <$> PL.signed P.empty PL.float

string :: Parser AST.Atom
string =
    lexeme $
        AST.AString . Text.pack
            <$> ( P.char '"'
                    *> P.manyTill PL.charLiteral (P.char '"')
                )

ident :: Parser AST.Atom
ident = AST.AIdent <$> ident'

ident' :: Parser AST.Identifier
ident' = lexeme $ Text.pack <$> P.some P.letterChar

typedIdent :: Parser AST.Atom
typedIdent = ident <* typeInfo

typedIdent' :: Parser AST.Identifier
typedIdent' = ident' <* typeInfo

-- Collections
collection :: Parser (AST.Expr P.SourcePos)
collection = P.choice [list, P.try dict, set, P.try tuple]

tuple :: Parser (AST.Expr P.SourcePos)
tuple =
    withPos AST.ETuple $ do
        P.between (symbol "(") (symbol ")") $ do
            xs <- expr `P.sepBy` symbol ","
            case xs of
                [] -> fail "expected non-empty tuple"
                [_] -> fail "expected non-singleton tuple"
                _ -> pure $ Vector.fromList xs

list :: Parser (AST.Expr P.SourcePos)
list =
    withPos AST.EList $
        P.between (symbol "[") (symbol "]") $
            Vector.fromList <$> (expr `P.sepBy` symbol ",")

set :: Parser (AST.Expr P.SourcePos)
set =
    withPos AST.ESet $
        P.between (symbol "{") (symbol "}") $
            Set.fromList <$> (expr `P.sepBy` symbol ",")

dict :: Parser (AST.Expr P.SourcePos)
dict =
    withPos AST.EDict $
        P.between (symbol "{") (symbol "}") $
            Map.fromList
                <$> ( ((,) <$> (expr <* symbol ":") <*> expr)
                        `P.sepBy` symbol ","
                    )
