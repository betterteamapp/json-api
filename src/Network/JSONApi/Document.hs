{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi.Document
  ( Document
  , OneDoc
  , ManyDoc
  , docData
  , docLinks
  , docMeta
  , docIncluded
  , ErrorDocument (..)
  , errorDoc
  , Included
  , getIncluded
  , oneDoc
  , manyDoc
  , composeDoc
  , include
  , includes
  , SparseFields(..)
  , queryParamSparseFields
  , makeSparseDocument
  -- , makeSparseResource
  ) where

import Data.Aeson
       (FromJSON, FromJSON1(..), ToJSON, ToJSON1(..), Value, (.:), (.:?),
        (.=), parseJSON1, toJSON1)
import Control.Lens hiding ((.=))
import Control.Lens.TH
import qualified Data.Aeson as AE
import qualified Data.DList as DL
import Data.Functor.Compose
import Data.Hashable
import Data.Hashable.Lifted
import Data.Foldable
import Data.Functor.Classes
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Generics as G
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link as L
import Network.JSONApi.Meta as M
import Network.JSONApi.Resource (Resource, ToResourcefulEntity, FromResourcefulEntity)
import qualified Network.JSONApi.Resource as R
import qualified Network.JSONApi.Identifier as I

{- |
The 'Document' type represents the top-level JSON-API requirement.

@data@ attribute - the resulting JSON may be either a singleton resource
or a list of resources. See 'Resource' for the construction.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document (st :: *) (f :: * -> *) (a :: *) = Document
  { _docData :: Compose f (R.Resource st) a
  , _docLinks :: Links
  , _docMeta :: Meta
  , _docIncluded :: [R.Resource I.Existing Value]
  } deriving (G.Generic, G.Generic1)

makeLenses ''Document

type OneDoc a = Document (R.ResourceIdState a) Identity (R.ResourceValue a)
type ManyDoc a = Document (R.ResourceIdState a) Vector (R.ResourceValue a)

instance (Show1 f, Show1 (Resource st), Show a, Show (I.ResourceState st a)) => Show (Document st f a) where
  showsPrec n v =
    showString "Document {data = " .
    showsPrec n (_docData v) .
    showString ", links = " .
    showsPrec n (_docLinks v) .
    showString ", meta = " .
    showsPrec n (_docMeta v) .
    showString ", included = " .
    showsPrec n (_docIncluded v) .
    showString "}"

instance (Eq1 f, Eq a, Eq1 (Resource st), Eq (I.ResourceState st a)) => Eq (Document st f a) where
  (==) a b =
    _docData a == _docData b &&
    _docLinks a == _docLinks b &&
    _docMeta a == _docMeta b &&
    _docIncluded a == _docIncluded b

instance (Hashable1 f, Hashable a, Hashable1 (Resource st), Hashable (I.ResourceState st a)) => Hashable (Document st f a) where
  hashWithSalt s x =
    s `hashWithSalt`
    _docData x `hashWithSalt`
    _docLinks x `hashWithSalt`
    _docMeta x `hashWithSalt` _docIncluded x

deriving instance (Functor f) => Functor (Document st f)

instance (ToJSON1 f, ToJSON a, ToJSON1 (Resource st)) => ToJSON (Document st f a) where
  toJSON (Document vs links meta included) = AE.object
    (("data" .= toJSON1 vs) : optionals)
    where
      optionals = catMaybes
        [ if (HM.null $ fromLinks links) then Nothing else Just ("links" .= links)
        , if (HM.null $ fromMeta meta) then Nothing else Just ("meta"  .= meta)
        , if (null included) then Nothing else Just ("included" .= included)
        ]

instance (FromJSON1 f, FromJSON a, FromJSON1 (Resource st)) => FromJSON (Document st f a) where
  parseJSON = AE.withObject "document" $ \v -> do
    dat <- v .:  "data"
    d <- parseJSON1 dat
    l <- v .:? "links"
    m <- v .:? "meta"
    i <- v .:? "included"
    return $ Document
      d
      (fromMaybe mempty l)
      (fromMaybe mempty m)
      (fromMaybe [] i)

{- |
The 'Included' type is an abstraction used to constrain the @included@
section of the Document to JSON serializable Resource objects while
enabling a heterogeneous list of Resource types.

No data constructors for this type are exported as we need to
constrain the 'Value' to a heterogeneous list of Resource types.
See 'mkIncludedResource' for creating 'Included' types.
-}
newtype Included = Included (DL.DList (Resource I.Existing Value))
  deriving (Show, Semigroup, Monoid)

getIncluded :: Included -> [Resource I.Existing Value]
getIncluded (Included d) = DL.toList d

{- |
Constructor function for the Document data type.
-}
oneDoc :: (ToJSON (R.ResourceValue a), ToResourcefulEntity m a) => a -> m (OneDoc a)
oneDoc = fmap (composeDoc . pure) . R.toResource

{- |
Constructor function for the Document data type.
-}
manyDoc :: (Monad m, ToJSON (R.ResourceValue a), ToResourcefulEntity m a) => [a] -> m (ManyDoc a)
manyDoc = fmap composeDoc . mapM R.toResource . V.fromList

{- |
Constructor function for the Document data type. It is possible to create an
invalid Document if the provided @f@ doesn't serialize to either single
@ResourceValue@ or an array of @ResourceValue@s
-}
composeDoc :: f (Resource st a) -> Document st f a
composeDoc functor = Document (Compose functor) mempty mempty mempty

{- |
Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
include ::
     ( AE.ToJSON (R.ResourceValue a)
     , ToResourcefulEntity m a
     , R.ResourceIdState a ~ I.Existing
     )
  => a
  -> m Included
include = fmap (Included . DL.singleton . fmap AE.toJSON) . R.toResource

{- |
Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
includes ::
     ( Foldable f
     , AE.ToJSON (R.ResourceValue a)
     , Monad m
     , ToResourcefulEntity m a
     , R.ResourceIdState a ~ I.Existing
     )
  => f a
  -> m Included
includes = fmap (Included . DL.fromList . fmap (fmap AE.toJSON)) . mapM R.toResource . toList

newtype SparseFields = SparseFields
  { sparseFields :: HM.HashMap Text (HS.HashSet Text)
  } deriving (Show, Eq)

queryParamSparseFields :: [(Text, Text)] -> SparseFields
queryParamSparseFields fs =
  SparseFields $
  HM.fromList [r | f <- fs, r <- toList $ pullField f]
  where
    pullField (wrappedK, v) = do
      k' <- T.stripPrefix "fields[" wrappedK
      k <- T.stripSuffix "]" k'
      return (k, HS.fromList $ T.splitOn "," v)

{- |
Documents MUST respect sparse field requests, so this provides the necessary machinery.

> GET /articles?include=author&fields[articles]=title,body&fields[people]=name HTTP/1.1

-}
-- Only types noted in the hashmap are filtered
makeSparseResource :: (ToJSON a) => SparseFields -> Resource st a -> Resource st Value
makeSparseResource (SparseFields subsets) x =
  case HM.lookup (x ^. R.resIdentifier . I.datatype) subsets of
    Nothing -> x & R.resValue .~ o
    Just filters ->
      x & R.resValue .~
      AE.Object (HM.filterWithKey (\k _ -> HS.member k filters) j)
  where
    o@(AE.Object j) = x ^. R.resValue . to AE.toJSON

{- |
Documents MUST respect sparse field requests, so this provides the necessary machinery.

> GET /articles?include=author&fields[articles]=title,body&fields[people]=name HTTP/1.1

-}
makeSparseDocument :: (Functor f, ToJSON a) => SparseFields -> Document st f a -> Document st f Value
makeSparseDocument subsets d = d
  & docData . _Wrapped . mapped %~ makeSparseResource subsets
  & docIncluded . mapped %~ makeSparseResource subsets

{- |
The 'ErrorDocument' type represents the alternative form of the top-level
JSON-API requirement.

@error@ attribute - a descriptive object encapsulating application-specific
error detail.

For more information see: <http://jsonapi.org/format/#errors>
-}
data ErrorDocument a = ErrorDocument
  { _errors :: [E.Error a]
  , _errorLinks :: Links
  , _errorMeta :: Meta
  } deriving (Show, Eq, G.Generic)

instance ToJSON (ErrorDocument a) where
  toJSON (ErrorDocument err links meta) = AE.object $ catMaybes
    [ Just ("errors" .= err)
    , if (null $ fromLinks links) then Nothing else Just ("links" .= links)
    , if (HM.null $ fromMeta meta) then Nothing else Just ("meta"  .= meta)
    ]

instance FromJSON (ErrorDocument a) where
  parseJSON = AE.withObject "errors" $ \v -> do
    e <- v .: "errors"
    l <- v .:? "links"
    m <- v .:? "meta"
    return $ ErrorDocument e (fromMaybe mempty l) (fromMaybe mempty m)

errorDoc :: [E.Error a] -> ErrorDocument a
errorDoc es = ErrorDocument es mempty mempty
