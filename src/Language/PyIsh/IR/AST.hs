module Language.PyIsh.IR.AST (
    Identifier,
    Atom (..),
    Expr (..),
    Statement (..),
    PrimUnaryOp (..),
    PrimBinOp (..),
    showAtom,
    showPrimUnaryOp,
    showPrimBinOp,
    showExpr,
    showStatement,
) where

import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Language.PyIsh.Utils

type Identifier = Text.Text

data Atom
    = AInt Integer
    | AFloat Double
    | ABool Bool
    | AString Text.Text
    | AIdent Identifier
    deriving (Show, Read, Eq, Ord)

showAtom :: Atom -> Text.Text
showAtom = \case
    AInt i -> Text.pack $ show i
    AFloat f -> Text.pack $ show f
    ABool b -> Text.pack $ show b
    AString s -> s
    AIdent i -> i

data PrimUnaryOp
    = Negate
    deriving (Show, Read, Eq, Ord)

showPrimUnaryOp :: PrimUnaryOp -> Text.Text
showPrimUnaryOp = \case
    Negate -> "-"

data PrimBinOp
    = -- math
      Add
    | Sub
    | Mul
    | Div
    | Exp
    | -- bools - compares
      Eq
    | NotEq
    | Less
    | LessEq
    | Greater
    | GreaterEq
    | -- bools - combines
      And
    | Or
    | Xor
    deriving (Show, Read, Eq, Ord)

showPrimBinOp :: PrimBinOp -> Text.Text
showPrimBinOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Exp -> "**"
    Eq -> "=="
    NotEq -> "!="
    Less -> "<"
    LessEq -> "<="
    Greater -> ">"
    GreaterEq -> ">="
    And -> "and"
    Or -> "or"
    Xor -> "^"

data Expr a
    = EAtom a Atom
    | EPrimUnaryOp a PrimUnaryOp (Expr a)
    | EPrimBinOp a PrimBinOp (Expr a) (Expr a)
    | EApply a Identifier [Expr a]
    | ETuple a (Vector.Vector (Expr a))
    | EList a (Vector.Vector (Expr a))
    | EDict a (Map.Map (Expr a) (Expr a))
    | ESet a (Set.Set (Expr a))
    | ESplat a (Expr a)
    deriving (Show, Read, Eq, Ord)

showExpr :: Expr a -> Text.Text
showExpr = \case
    EAtom _ a ->
        showAtom a
    EPrimUnaryOp _ op e ->
        Text.concat ["(", showPrimUnaryOp op, showExpr e, ")"]
    EPrimBinOp _ op e1 e2 ->
        Text.concat ["(", showExpr e1, showPrimBinOp op, showExpr e2, ")"]
    EApply _ f xs ->
        f <> "(" <> innerCol showExpr xs <> ")"
    ETuple _ xs ->
        let inner = innerCol showExpr $ Vector.toList xs
         in "(" <> inner <> ")"
    EList _ xs ->
        let inner = innerCol showExpr $ Vector.toList xs
         in "[" <> inner <> "]"
    EDict _ xs ->
        let into (k, v) = k <> ": " <> v
            inner = innerCol (into . both showExpr) $ Map.toList xs
         in "{" <> inner <> "}"
    ESet _ xs ->
        let inner = innerCol showExpr $ Set.toList xs
         in "{" <> inner <> "}"
    ESplat _ e -> '*' `Text.cons` showExpr e
  where
    innerCol f xs = Text.intercalate ", " $ f <$> xs

data Statement a
    = SDefine a Identifier [Identifier] [Statement a]
    | SIf a (Expr a, [Statement a]) [(Expr a, [Statement a])] (Maybe [Statement a])
    | SAssign a Identifier (Expr a)
    | SReturn a (Expr a)
    | SYield a (Expr a)
    | SExpr a (Expr a)
    deriving (Show, Read, Eq, Ord)

showStatement :: Statement a -> Text.Text
showStatement = join showStatement' 4

showStatement' :: Int -> Int -> Statement a -> Text.Text
showStatement' step =
    go
  where
    go :: Int -> Statement a -> Text.Text
    go n = \case
        SDefine _ f ns bs ->
            Text.concat
                [ top "def " (f <> "(" <> Text.intercalate ", " ns <> ")")
                , showBody bs
                ]
        SIf _ (iff, ifb) elifs elseb ->
            let if_ = top "if " (showExpr iff) <> showBody ifb
                elif_ :: Text.Text
                elif_ =
                    Text.concat $
                        (\(f, b) -> top "elif " (showExpr f) <> showBody b)
                            <$> elifs
                else_ = maybe "" ((top "else" "" <>) . showBody) elseb
             in Text.intercalate (newline' (n - step)) [if_, elif_, else_]
        SAssign _ i e -> i <> " = " <> showExpr e
        SReturn _ e -> "return " <> showExpr e
        SYield _ e -> "yield " <> showExpr e
        SExpr _ e -> showExpr e
      where
        top :: Text.Text -> Text.Text -> Text.Text
        top prefix inner = prefix <> inner <> ":" <> newline

        newline = newline' n
        newline' x = Text.pack $ '\n' : replicate x ' '

        showBody xs = Text.intercalate newline $ go (n + step) <$> xs
