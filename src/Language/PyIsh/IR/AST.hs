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

import Control.Arrow (Arrow (..))
import Control.Monad (join)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

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
    ABool b -> if b then "true" else "false"
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
    | SIf a (Expr a, [Statement a]) [(Expr a, [Statement a])] (Maybe [Statement a])
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
             in Text.intercalate (newline' (n - step)) $ [if_, elif_, else_]
        SExpr _ e -> showExpr e
      where
        top :: Text.Text -> Text.Text -> Text.Text
        top prefix inner = prefix <> inner <> ":" <> newline

        next = go (n + step)

        newline = newline' n
        newline' x = Text.pack $ '\n' : (replicate x ' ')

        showBody xs = Text.intercalate newline $ go (n + step) <$> xs
