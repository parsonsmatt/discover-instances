{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.PolyClass where

import Data.Typeable

class (Typeable a, Typeable k) => PolyClass (a :: k) where
    stringOf :: proxy a -> String

instance PolyClass Int where
    stringOf _ = "int"

instance PolyClass Maybe where
    stringOf _ = "maybe"
