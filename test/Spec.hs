{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -ddump-splices #-}

import Data.Foldable
import Data.Proxy
import Data.Typeable
import DiscoverInstances

show :: [SomeDict Show]
show = $$(discoverInstances @Show)

eq :: [SomeDict Eq]
eq = $$(discoverInstances)

eqShow :: [(SomeDict Eq, SomeDict Show)]
eqShow = $$(discoverBothInstances)

functor :: [SomeDict Functor]
functor = $$discoverInstances

main :: IO ()
main = do
    for_ functor $ \(SomeDictOf p@(Proxy :: Proxy t)) -> do
        print 1234
