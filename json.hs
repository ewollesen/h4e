{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}
import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Text.Printf
import Control.Monad
import Equipment
import Modifier
import Taggable
import System.IO


data Foo = Foo { word :: String } deriving (Show)
$(derive[''Foo])

a = Foo { word="bar" }
b = toJsonString a
c = fromJsonString (undefined :: Foo) b

getIt :: Either String a -> a
getIt c = case c of Right x -> x
                    Left e -> error e


$(derive[''Equipment])
$(derive[''Modifier])
$(derive[''Tag])

--plateMail
--print $ toJsonString plateMail

pm = toJsonString plateMail
pm' = getIt $ fromJsonString (undefined :: Equipment) pm

dumpIt e f = do
  writeFile f $ toJsonString e