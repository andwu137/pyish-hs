module Language.PyIsh.Utils (
    both,
    (.*),
) where

import Control.Monad
import Data.Bifunctor

both :: (Bifunctor p) => (a -> b) -> p a a -> p b b
both = join bimap

infixr 8 .*
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = fmap . fmap
