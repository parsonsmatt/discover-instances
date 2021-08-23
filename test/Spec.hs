{-# language TemplateHaskell, PolyKinds, ScopedTypeVariables, TypeApplications #-}

{-# OPTIONS_GHC -ddump-splices #-}

import DiscoverInstances
import Data.Proxy
import Data.Typeable
import Data.Foldable

show :: [SomeDict Show]
show = $$(discoverInstances @Show)

eq :: [SomeDict Eq]
eq = $$(discoverInstances)

functor :: [SomeDict Functor]
functor = $$(discoverInstances)

main :: IO ()
main = do
    for_ functor $ \(SomeDictOf p@(Proxy :: Proxy t)) -> do
        print 1234
