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
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

type Identifier = Text.Text

data Atom
    = AInt Integer
    | AFloat Double
    | AString Text.Text
    | AIdent Identifier
    deriving (Show, Read, Eq, Ord)

showAtom :: Atom -> Text.Text
showAtom = \case
    AInt i -> Text.pack $ show i
    AFloat f -> Text.pack $ show f
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
    Eq -> "=="
    NotEq -> "!="
    Less -> "<"
    LessEq -> "<="
    Greater -> ">"
    GreaterEq -> ">="
    And -> "and"
    Or -> "or"
    Xor -> "xor"

data Expr a
    = EAtom a Atom
    | EPrimUnaryOp a PrimUnaryOp (Expr a)
    | EPrimBinOp a PrimBinOp (Expr a) (Expr a)
    | EApply a Identifier [Expr a]
    | ETuple a (Vector.Vector (Expr a))
    | EList a (Vector.Vector (Expr a))
    | EDict a (Map.Map (Expr a) (Expr a))
    | ESet a (Set.Set (Expr a))
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
        f <> "(" <> Text.intercalate ", " (showExpr <$> xs) <> ")"
    ETuple _ xs ->
        undefined
    EList _ xs ->
        undefined
    EDict _ xs ->
        undefined
    ESet _ xs ->
        undefined

data Statement a
    = SDefine a Identifier [Identifier] [Statement a]
    | SExpr a (Expr a)
    deriving (Show, Read, Eq, Ord)
