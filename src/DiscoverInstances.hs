{-# language LambdaCase, RankNTypes, PartialTypeSignatures, TemplateHaskell, ConstraintKinds, KindSignatures, PolyKinds #-}
{-# language ImportQualifiedPost, ScopedTypeVariables, TypeApplications, GADTs #-}

module DiscoverInstances where

import Data.Proxy
import GHC.Exts
import Data.Kind hiding (Type)
import Data.Kind qualified as Kind
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Typeable
import LiftType

type TyPe = Kind.Type

data InstanceDict (c :: k -> Constraint) where
    InstanceDict :: c a => Proxy a -> InstanceDict c

-- |
--
-- @since 0.1.0.0
discoverInstances :: forall (c :: _ -> Constraint) . (Typeable c) => Q (TExp [InstanceDict c])
discoverInstances = do
    let
        className =
            show (typeRep (Proxy @c))
    instanceDecs <- reifyInstances (mkName className) [VarT (mkName "a")]

    dicts <- fmap listTE $ traverse decToDict instanceDecs

    [|| concat $$(pure dicts) ||]

listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType

decToDict :: forall k (c :: k -> Constraint). InstanceDec -> Q (TExp [InstanceDict c])
decToDict = \case
    InstanceD _moverlap cxt typ _decs ->
        case cxt of
            [] -> do
                let
                    t =
                       case typ of
                           AppT _ t ->
                               t
                    proxy =
                        [| Proxy :: Proxy $(pure t) |]
                unsafeTExpCoerce [| [ InstanceDict $proxy ] |]
            _ -> do
                reportWarning $
                    "I haven't figured out how to put constrained instances on here, so I'm skipping the type: " <> show typ
                [|| [] ||]

