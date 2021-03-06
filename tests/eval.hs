{-# LANGUAGE QuasiQuotes #-}
import Language.Zephyr.Eval
import Language.Zephyr.TypeCheck
import Language.Zephyr.Quote
import Data.Default
import Data.Reflection

Right expr0 = runTypeCheck $ typeExpr def [zephyrExp|42|]

Right expr1 = runTypeCheck $ typeExpr def [zephyrExp|\g -> g 42|]
