{-# language LambdaCase, RankNTypes, AllowAmbiguousTypes, PartialTypeSignatures, QuantifiedConstraints, TemplateHaskell, ConstraintKinds, KindSignatures, PolyKinds #-}
{-# language ScopedTypeVariables, TypeApplications, GADTs #-}

-- | Generally speaking, this module is only useful to discover instances
-- of unary type classes where the instance is unconstrained.
--
-- That is to say - not all that useful in libraries.
--
-- However, this has ended up being super useful to me in a bunch of
-- application contexts. Consider @persistent-discover@, which grabs all
-- the @PersistEntity@ instances in scope and makes the @[EntityDef]@ for
-- them. Or consider a front-end types generating module, which needs to
-- import a ton of modules, and then call @toSomeFrontEndClass :: SomeClass
-- a => Proxy a -> SomeThing@ on each thing.
--
-- This library can simplify that process.
--
-- @since 0.1.0.0
module DiscoverInstances
    (
    -- * The main interface
      discoverInstances
    -- * Using the results of 'discoverInstances'
    , module SomeDictOf
    -- * Re-exports
    , module Data.Proxy
    ) where

import Data.Proxy
import GHC.Exts
import Data.Kind hiding (Type)
import qualified Data.Kind as Kind
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Typeable
import LiftType
import Data.Constraint
import SomeDictOf

-- | This TemplateHaskell function accepts a type and splices in a list of
-- 'SomeDict's that provide evidence that the type is an instance of
-- the class that you asked for.
--
-- There are some limitations.
--
-- * The class can only accept a single parameter.
-- * The instances returned do not have a context.
--
-- @since 0.1.0.0
discoverInstances :: forall (c :: _ -> Constraint) . (Typeable c) => Q (TExp [SomeDict c])
discoverInstances = do
    let
        className =
            show (typeRep (Proxy @c))
    instanceDecs <- reifyInstances (mkName className) [VarT (mkName "a")]

    dicts <- fmap listTE $ traverse decToDict instanceDecs

    [|| concat $$(pure dicts) ||]

listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType

decToDict :: forall k (c :: k -> Constraint). InstanceDec -> Q (TExp [SomeDict c])
decToDict = \case
    InstanceD _moverlap cxt typ _decs ->
        case cxt of
            [] -> do
                let
                    t =
                       case typ of
                           AppT _ t ->
                               stripSig t
                    stripSig (SigT a b) =
                        a
                    stripSig x =
                        x
                    proxy =
                        [| Proxy :: Proxy $(pure t) |]
                unsafeTExpCoerce [| [ SomeDictOf $proxy ] |]
            _ -> do
                -- reportWarning $
                --     "I haven't figured out how to put constrained instances on here, so I'm skipping the type: "
                --     <> show typ
                --     <> ", context: "
                --     <> show cxt
                [|| [] ||]
