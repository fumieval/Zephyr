{-# LANGUAGE FlexibleContexts #-}
module Language.Zephyr.Eval (eval, match) where
import Unsafe.Coerce
import GHC.Prim
import Language.Zephyr.Syntax
import Language.Zephyr.Pretty
import Control.Monad
import Data.Reflection
import Debug.Trace
import qualified Data.Map as Map

-- TODO: make it safe by Typeable

eval :: [Any] -> Expr' (Type Kind) -> (a, Type Kind)
eval _ (t :< HoleE) = error $ "Hole: " ++ show (prettyType t)
eval vs (t :< BoundE i) = (unsafeCoerce (vs !! i), t)
eval vs (t :< AppE a b) = (v w, t) where
    (v, _) = eval vs a
    (w, _) = eval vs b
eval _ (t :< LambdaE []) = error "empty case"
eval vs (t :< LambdaE cs@(Clause ps' _ : _)) = flip (,) t $ unsafeCoerce $ argOf (length ps') $ \xs -> maybe (error "Non-exhaustive") id
    $ msum $ flip map cs $ \(Clause ps r) -> do
    bs <- matches ps xs
    return $ fst $ eval (bs ++ vs) r
eval vs (t :< LitE lit) = (fromLit lit, t)

fromLit :: Lit -> a
fromLit (IntegerL i) = unsafeCoerce i
fromLit (StringL s) = unsafeCoerce s

matches :: [Pat a] -> [Any] -> Maybe [Any]
matches ps vs = fmap concat $ zipWithM match ps vs

match :: Pat a -> Any -> Maybe [Any]
match (_ :< WildP) _ = return []
match (_ :< VarP _) v = return [v]
match (_ :< ConP n ps) v = do
    let (con, vs) = unsafeCoerce v
    guard $ n == con
    matches ps vs
match (_ :< LitP lit) v = if compareLit lit v then return [] else mzero

argOf :: Int -> ([Any] -> Any) -> Any
argOf m f = go m [] where
    go 0 vs = f vs
    go n vs = unsafeCoerce $ \v -> go (n - 1) (v:vs)

compareLit :: Lit -> Any -> Bool
compareLit (IntegerL x) v = unsafeCoerce v == x
compareLit (StringL x) v = unsafeCoerce v == x