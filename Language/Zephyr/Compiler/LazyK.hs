module Language.Zephyr.Compiler.LazyK where

import Language.Zephyr.Internal.Combinator

compile :: Free SKI String
compile (VarE v) = Pure (Right v)
compile (BoundE v) = Pure (Left v)
compile (AppE a b) = compile a $$ compile b
compile (LitE lit) = encodeLit
compile (HoleE h) = (S $$ I $$ I) (S $$ I $$ I)
compile (LambdaE cs) = where
    [foldr bindee vs e | Clause p e <- cs, p ^.. patVars . _2]