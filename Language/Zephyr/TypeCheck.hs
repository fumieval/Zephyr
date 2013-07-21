{-# LANGUAGE Rank2Types, DeriveFunctor, OverloadedStrings #-}
module Language.Zephyr.TypeCheck where
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Language.Zephyr.Lens
import Language.Zephyr.Quote
import Language.Zephyr.Syntax
import Language.Zephyr.Pretty
import Text.PrettyPrint
import qualified Data.Map as Map

data TypeCheckBase a = TypeError Doc
    | Subst TyVar Type a
    | Apply Type (Type -> a)
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

varBind :: TyVar -> Type -> TypeCheck ()
varBind v t
    | t == VarT v = return ()
    | elemOf tyvars v t = typeError "occurs check failed"
    | otherwise = liftF $ Subst v t ()

unify :: Type -> Type -> TypeCheck ()
unify ta tb = join $ typeExpr <$> apply ta <*> apply tb where
    typeExpr (AppT s t) (AppT u v) = do
        typeExpr s u
        unify t v
    typeExpr (VarT s) t = varBind s t
    typeExpr s (VarT t) = varBind t s
    typeExpr (ConT s) (ConT t) | s == t = return ()
    typeExpr s t = typeError $ text "Failed to unify " <> prettyType s <> text " with " <> prettyType t

apply :: Type -> TypeCheck Type
apply t = liftF $ Apply t id

applyExpr :: Expr Type -> TypeCheck (Expr Type)
applyExpr (t :< b) = (:<) <$> apply t <*> traverse applyExpr b

applyPat :: Pat Type -> TypeCheck (Pat Type)
applyPat (t :< b) = (:<) <$> apply t <*> traverse applyPat b

arrT :: Type -> Type -> Type
arrT s t = AppT (AppT ArrT s) t

exprTypeOf :: Type -> Expr Type -> TypeCheck (Expr Type)
exprTypeOf t e = do
    unify t (extract e)
    applyExpr e

type Binding = Map.Map Name Type

unionBinding :: Binding -> Binding -> TypeCheck Binding
unionBinding a b = do
    mapM_ (uncurry unify) $ Map.elems $ Map.intersectionWith (,) a b
    return $ Map.union a b

typeExpr :: Binding -> Expr a -> TypeCheck (Expr Type)
typeExpr bs (_ :< expr) = case expr of
    SigE t e -> typeExpr bs e >>= exprTypeOf t
    VarE s -> case bs ^? ix s of
        Just t -> return $ t :< VarE s
        Nothing -> do
            n <- freshName
            return $ VarT n :< VarE s
    AppE uf ug -> do
        a <- VarT <$> freshName
        b <- VarT <$> freshName

        g <- typeExpr bs ug >>= exprTypeOf a
        f <- typeExpr bs uf >>= exprTypeOf (a `arrT` b)

        return $ b :< AppE f g
    LambdaE [Clause ups ue] -> do
        ps <- mapM (typePat bs) ups -- slack
        bindings <- unionBinding bs $ Map.fromList (ps ^.. traverse . patVars)
        e <- typeExpr bindings ue >>= applyExpr
        ps' <- mapM applyPat ps
        return $ foldr (\p r -> extract p `arrT` r) (extract e) ps' :< LambdaE [Clause ps' e]
    LitE (IntegerL i) -> return $ ConT "Int" :< LitE (IntegerL i)
    LitE (StringL i) -> return $ ConT "String" :< LitE (StringL i)

typePat :: Binding -> Pat a -> TypeCheck (Pat Type)
typePat _ (_ :< WildP) = do
    n <- freshName
    return $ VarT n :< WildP
typePat _ (_ :< VarP v) = do
    n <- freshName
    return $ VarT n :< VarP v
typePat bs (_ :< SigP t up) = do
    p <- typePat bs up
    unify t (extract p)
    applyPat p
typePat bs (_ :< ConP name ups) = case bs ^? ix name of
    Nothing -> typeError ("Not in scope:" <+> prettyName name)
    Just t -> do
        ps <- mapM (typePat bs) ups
        c <- go 0 t ps
        ps' <- mapM applyPat ps
        return $ c :< ConP name ps'
    where
        go n (AppT (AppT ArrT s) t) (p : ps) = do
            unify (extract p) s
            go (n + 1) t ps
        go n (ConT t) [] = return (ConT t)
        go n _ _ = typeError $ "Constructor"
            <+> prettyName name
            <+> "should have"
            <+> int n
            <+> "argument, but it has been given"
            <+> int (length ups)