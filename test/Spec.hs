{-# language TemplateHaskell, TypeApplications #-}

import DiscoverInstances

show :: [InstanceDict Show]
show = $$(discoverInstances @Show)

eq :: [InstanceDict Eq]
eq = $$(discoverInstances)

functor :: [InstanceDict Functor]
functor = $$(discoverInstances)

main :: IO ()
main = do
    putStrLn "okay"
