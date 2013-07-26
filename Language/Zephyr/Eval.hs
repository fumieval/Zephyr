{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Language.Zephyr.Eval (Runtime(..), toTypeRep, eval, evalCore, match) where
import Unsafe.Coerce
import GHC.Prim
import Language.Zephyr.Syntax
import Language.Zephyr.Pretty
import Control.Monad
import Data.Reflection
import Debug.Trace
import qualified Data.Map as Map
import Data.Typeable
import Control.Applicative
import Control.Lens
import Data.Default

-- TODO: earlier unification

data Runtime = Runtime {
    _primTyCons :: Map.Map Name TypeRep
    }
makeLenses ''Runtime

instance Default Runtime where
    def = Runtime
        { _primTyCons = Map.fromList [("Int", typeOf (undefined :: Int)), ("String", typeOf (undefined :: String))]
        }

toTypeRep :: Given Runtime => Type k -> Either String TypeRep
toTypeRep = go . unwrap where
    go ArrT = Right $ typeOf2 id
    go (VarT _) = Left "unexpected polymorphic type"
    go (ConT con) = maybe (Left "undefiend type constructor") Right $ given ^? primTyCons . ix con 
    go (AppT a b) = mkAppTy <$> toTypeRep a <*> toTypeRep b
    go (ForallT _ _ _) = Left "unexpected quantification"
    go (SigT _ _) = Left "unexpected signature"

eval :: (Typeable a, Given Runtime) => Expr' (Type k) -> Either String a
eval expr@(t :< _) = r where
    asRight :: a -> Either b a -> a
    asRight = const
    r = case toTypeRep t of
        Left err -> Left err
        Right ty | ty == typeOf (asRight undefined r) -> Right $ evalCore [] expr
                 | otherwise -> Left "type mismatch"

evalCore :: [Any] -> Expr' (Type k) -> a
evalCore vs (t :< base) = case base of
    HoleE -> error $ "Hole: " ++ show (prettyType t)
    BoundE i -> unsafeCoerce (vs !! i)
    AppE a b -> (evalCore vs a) (evalCore vs b)
    LambdaE [] -> error "empty case"
    LambdaE cs@(Clause ps' _ : _) -> unsafeCoerce
        $ argOf (length ps')
        $ \xs -> maybe (error "Non-exhaustive") id
        $ msum $ flip map cs $ \(Clause ps r) -> do
        bs <- matches ps xs
        return $ evalCore (bs ++ vs) r
    LitE lit -> fromLit lit

fromLit :: Lit -> a
fromLit (IntegerL i) = unsafeCoerce i
fromLit (StringL s) = unsafeCoerce s

matches :: [Pat t] -> [Any] -> Maybe [Any]
matches ps vs = fmap concat $ zipWithM match ps vs

match :: Pat t -> Any -> Maybe [Any]
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