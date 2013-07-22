{-# LANGUAGE Rank2Types, DeriveFunctor, OverloadedStrings, TemplateHaskell #-}
module Language.Zephyr.TypeCheck where
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Data.Default
import Language.Zephyr.Lens
import Language.Zephyr.Quote
import Language.Zephyr.Syntax
import Language.Zephyr.Pretty
import Text.PrettyPrint
import qualified Data.Map as Map

data TypeCheckBase a = TypeError Doc
    | Subst TyVar (Type Kind) a
    | Apply (Type Kind) (Type Kind -> a)
    | FreshName (TyVar -> a) deriving Functor

runTypeCheck :: TypeCheck a -> Either Doc a
runTypeCheck = typeExpr 0 [] where
    typeExpr i ss (Free (Apply t cont)) = typeExpr i ss $ cont $ foldr (\(v, s) -> tyvar v .~ s) t ss
    typeExpr i ss (Free (TypeError s)) = Left s
    typeExpr i ss (Free (Subst v t cont)) = typeExpr i ((v, t) : ss) cont
    typeExpr i ss (Free (FreshName cont)) = typeExpr (succ i) ss $ cont (AutoVar i)
    typeExpr _ ss (Pure a) = return a

type TypeCheck = Free TypeCheckBase 

freshName :: TypeCheck TyVar
freshName = liftF $ FreshName id

typeError :: Doc -> TypeCheck a
typeError = liftF . TypeError

varBind :: Kind -> TyVar -> Type Kind -> TypeCheck ()
varBind k v t
    | t == k :< VarT v = return ()
    | elemOf tyvars v t = typeError "occurs check failed"
    | otherwise = liftF $ Subst v t ()

unify :: Type Kind -> Type Kind -> TypeCheck ()
unify ta tb = join $ uni <$> apply ta <*> apply tb where
    uni (j :< _) (k :< _) | j /= k = typeError $ "Kind mismatch:" <+> prettyKind j <+> "~" <+> prettyKind k
    uni (_ :< AppT s t) (_ :< AppT u v) = do
        uni s u
        unify t v
    uni (k :< VarT s) t = varBind k s t
    uni s (k :< VarT t) = varBind k t s
    uni (_ :< ConT s) (_ :< ConT t) | s == t = return ()
    uni (_ :< ArrT) (_ :< ArrT) = return ()
    uni s t = typeError $ "Failed to unify" <+> prettyType s <+> "with" <+> prettyType t

apply :: Type Kind -> TypeCheck (Type Kind)
apply t = liftF $ Apply t id

arrT :: Type Kind -> Type Kind -> Type Kind
arrT s t = StarK :< AppT (FunK StarK StarK :< AppT (FunK StarK (FunK StarK StarK) :< ArrT) s) t

exprTypeOf :: Type Kind -> Expr (Type Kind) (Type Kind) -> TypeCheck (Expr (Type Kind) (Type Kind))
exprTypeOf t e = do
    unify t (extract e)
    traverse apply e

data TypeEnv = TypeEnv
    { _typeBindings :: Map.Map Name (Type Kind)
    , _kindBindings :: Map.Map TyVar Kind
    , _tyconBindings :: Map.Map Name Kind
    }
makeLenses ''TypeEnv

instance Default TypeEnv where
    def = TypeEnv Map.empty Map.empty (Map.fromList [("Int", StarK)])

unionBindings :: Map.Map Name (Type Kind) -> TypeEnv -> TypeCheck TypeEnv
unionBindings a env = do
    mapM_ (uncurry unify) $ Map.elems $ Map.intersectionWith (,) a (_typeBindings env)
    return $ typeBindings %~ Map.union a $ env

kindType :: TypeEnv -> Type a -> TypeCheck (Type Kind) -- unification?
kindType env s = case unwrap s of
    ArrT -> return $ FunK StarK (FunK StarK StarK) :< ArrT
    AppT s' t' -> do
        s <- kindType env s'
        t <- kindType env t'
        case extract s of
            FunK _ b -> return (b :< AppT s t)
            _ -> typeError "Kind mismatch"
    ConT n -> case env ^? tyconBindings . ix n of
        Nothing -> typeError "Not in scope: type constructor"
        Just k -> return $ k :< ConT n
    VarT v -> case env ^? kindBindings . ix v of
        Nothing -> typeError "Not in scope: type variable"
        Just k -> return $ k :< VarT v
    SigT k t' -> do
        t <- kindType env t'
        if extract t == k
            then return t
            else typeError "Kind mismatch"
typeExpr :: TypeEnv -> Expr a a -> TypeCheck (Expr (Type Kind) (Type Kind))
typeExpr env (_ :< expr) = case expr of
    SigE t' e -> do
        t <- kindType env t'
        typeExpr env e >>= exprTypeOf t
    VarE s -> case env ^? typeBindings . ix s of
        Just t -> return $ t :< VarE s
        Nothing -> do
            n <- freshName
            return $ (StarK :< VarT n) :< VarE s
    AppE uf ug -> do
        a <- VarT <$> freshName
        b <- VarT <$> freshName

        g <- typeExpr env ug >>= exprTypeOf (StarK :< a)
        f <- typeExpr env uf >>= exprTypeOf ((StarK :< a) `arrT` (StarK :< b))

        return $ (StarK :< b) :< AppE f g
    LambdaE [Clause ups ue] -> do
        ps <- mapM (typePat env) ups -- slack
        bs <- unionBindings (Map.fromList (ps ^.. traverse . patVars)) env
        e <- typeExpr bs ue >>= traverse apply
        ps' <- mapM (traverse apply) ps
        return $ foldr (\p r -> extract p `arrT` r) (extract e) ps' :< LambdaE [Clause ps' e]
    LitE (IntegerL i) -> return $ (StarK :< ConT "Int") :< LitE (IntegerL i)
    LitE (StringL i) -> return $ (StarK :< ConT "String") :< LitE (StringL i)

typePat :: TypeEnv -> Pat a -> TypeCheck (Pat (Type Kind))
typePat _ (_ :< WildP) = do
    n <- freshName
    return $ (StarK :< VarT n) :< WildP
typePat _ (_ :< VarP v) = do
    n <- freshName
    return $ (StarK :< VarT n) :< VarP v
typePat env (_ :< SigP t' up) = do
    t <- kindType env t'
    p <- typePat env up
    unify t (extract p)
    traverse apply p
typePat env (_ :< ConP name ups) = case env ^? typeBindings . ix name of
    Nothing -> typeError ("Not in scope:" <+> prettyName name)
    Just t -> do
        ps <- mapM (typePat env) ups
        c <- go 0 t ps
        ps' <- mapM (traverse apply) ps
        return $ c :< ConP name ps'
    where
        go n (_ :< AppT (_ :< AppT (_ :< ArrT) s) t) (p : ps) = do
            unify (extract p) s
            go (n + 1) t ps
        go n (k :< ConT t) [] = return (k :< ConT t)
        go n _ _ = typeError $ "Constructor"
            <+> prettyName name
            <+> "should have"
            <+> int n
            <+> "argument, but it has been given"
            <+> int (length ups)