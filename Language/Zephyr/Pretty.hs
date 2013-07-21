module Language.Zephyr.Pretty where
import Text.PrettyPrint
import Language.Zephyr.Syntax

prettyName :: Name -> Doc
prettyName n = text n

prettyType :: Type -> Doc
prettyType (AppT (AppT ArrT s) t) = parens (prettyType s <+> text "->" <+> prettyType t)
prettyType ArrT = parens (text "->")
prettyType (AppT s t) = parens (prettyType s <+> prettyType t)
prettyType (VarT (UserVar v)) = prettyName v
prettyType (VarT (AutoVar n)) = text "#" <> int n
prettyType (ConT v) = text v