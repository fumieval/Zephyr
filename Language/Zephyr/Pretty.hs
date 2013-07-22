module Language.Zephyr.Pretty where
import Text.PrettyPrint
import Language.Zephyr.Syntax

prettyName :: Name -> Doc
prettyName n = text n

parensPrec :: Int -> Doc -> Int -> Doc
parensPrec m doc n | m < n = parens doc
parensPrec m doc n = doc

prettyType :: Type a -> Doc
prettyType = flip p 0 where
    p (_ :< AppT (_ :< AppT (_ :< ArrT) s) t) = parensPrec 7 $ p s 8 <+> text "->" <+> p t 7
    p (_ :< ArrT) = const $ parens (text "->")
    p (_ :< AppT s t) = parensPrec 8 $ p s 8 <+> p t 9
    p (_ :< VarT (UserVar v)) = const $ prettyName v
    p (_ :< VarT (AutoVar n)) = const $ text "#" <> int n
    p (_ :< ConT v) = const $ text v

prettyKind :: Kind -> Doc
prettyKind StarK = text "*"
prettyKind (FunK j k) = parens $ prettyKind j <+> text "->" <+> prettyKind k