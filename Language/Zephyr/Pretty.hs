module Language.Zephyr.Pretty where
import Text.PrettyPrint
import Language.Zephyr.Syntax

prettyName :: Name -> Doc
prettyName n = text n

prettyType :: Type a -> Doc
prettyType (_ :< AppT (_ :< AppT (_ :< ArrT) s) t) = parens (prettyType s <+> text "->" <+> prettyType t)
prettyType (_ :< ArrT) = parens (text "->")
prettyType (_ :< AppT s t) = parens (prettyType s <+> prettyType t)
prettyType (_ :< VarT (UserVar v)) = prettyName v
prettyType (_ :< VarT (AutoVar n)) = text "#" <> int n
prettyType (_ :< ConT v) = text v

prettyKind :: Kind -> Doc
prettyKind StarK = text "*"
prettyKind (FunK j k) = parens $ prettyKind j <+> text "->" <+> prettyKind k