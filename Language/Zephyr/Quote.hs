{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts, UndecidableInstances #-}
module Language.Zephyr.Quote where

import Control.Comonad.Cofree
import Data.Monoid
import Data.Reflection
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Quote
import Language.Zephyr.Syntax as Z
import Text.Trifecta

deriveLift ''Z.ExprBase
deriveLift ''Z.PatBase
deriveLift ''Z.TyVar
deriveLift ''Z.Type
deriveLift ''Z.Lit
deriveLift ''Z.Clause

instance (Lift (f (Cofree f a)), Lift a) => Lift (Cofree f a) where
    lift (a :< f) = conE '(:<) `appE` lift a `appE` lift f

zephyrExp :: QuasiQuoter
zephyrExp = QuasiQuoter { quoteExp = parse
    , quotePat = const $ fail "Unsupported"
    , quoteType = const $ fail "Unsupported"
    , quoteDec = const $ fail "Unsupported" } where
    parse s = case parseString (give (ParseEnv [] []) parseExpr) mempty s of
        Success a -> lift a
        Failure err -> fail (show err)