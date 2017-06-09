{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Resource
( Resource
, resIdentifier
, resValue
, resLinks
, resRelationships
, Relationships
, ResourcefulEntity (..)
, Relationship
, RelationshipType(..)
, mkRelationship
, mkRelationships
) where

import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.These (These(..))
import GHC.Generics hiding (Meta)
import Network.JSONApi.Identifier (HasIdentifier (..), Identifier (..))
import Network.JSONApi.Link (Links(..))
import Network.JSONApi.Meta (Meta(..))
import Prelude hiding (id)
import qualified Prelude

{- |
A type representing the Relationship between 2 entities

A Relationship provides basic information for fetching further information
about a related resource.

Specification: <http://jsonapi.org/format/#document-resource-object-relationships>
-}
data Relationship = Relationship
  { _data :: Maybe (RelationshipType Identifier)
  , _links :: Links
  } deriving (Show, Eq, Generic)

instance ToJSON Relationship where
  toJSON = AE.genericToJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

instance FromJSON Relationship where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }


newtype Relationships = Relationships { fromRelationships :: HM.HashMap Text Relationship }
  deriving (Show, Eq, Generic, Monoid, ToJSON, FromJSON)

data RelationshipType a
  = ToOne (Maybe a)
  | ToMany [a]
  deriving (Show, Eq, Functor)

instance (ToJSON a) => ToJSON (RelationshipType a) where
  toJSON rel = case rel of
    ToOne mval -> case mval of
      Nothing -> AE.Null
      Just a -> AE.toJSON a
    ToMany vals -> AE.toJSON vals

instance (FromJSON a) => FromJSON (RelationshipType a) where
  parseJSON AE.Null = pure $ ToOne Nothing
  parseJSON o@(AE.Object _) = (ToOne . Just) <$> AE.parseJSON o
  parseJSON a@(AE.Array _) = ToMany <$> AE.parseJSON a
  parseJSON wat = AE.typeMismatch "RelationshipType" wat

{- |
Type representing a JSON-API resource object.

A Resource supplies standardized data and metadata about a resource.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
data Resource a = Resource
  { _resIdentifier :: Identifier
  , _resValue :: a
  , _resLinks :: Links
  , _resRelationships :: Relationships
  } deriving (Show, Eq, Generic)

makeFields ''Resource

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj linksObj rels) =
    AE.object (["type" .= resType, "attributes" .= resObj] ++ optionals)
    where
      optionals = catMaybes
        [ ("id" .=) <$> resId
        , if (HM.null $ fromLinks linksObj) then Nothing else Just ("links" .= linksObj)
        , if (HM.null $ fromMeta metaObj) then Nothing else Just ("meta" .= metaObj)
        , if (HM.null $ fromRelationships rels) then Nothing else Just ("relationships" .= rels)
        ]

instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = AE.withObject "resourceObject" $ \v -> do
    id    <- v .:? "id"
    typ   <- v .: "type"
    attrs <- v .: "attributes"
    links <- v .:? "links"
    meta  <- v .:? "meta"
    rels  <- v .:? "relationships"
    return $ Resource
      (Identifier id typ $ fromMaybe mempty meta)
      attrs
      (fromMaybe mempty links)
      (fromMaybe mempty rels)

instance HasIdentifier (Resource a) where
  identifier = _resIdentifier

{- |
A typeclass for decorating an entity with JSON API properties
-}
class ResourcefulEntity a where
  type ResourceValue a :: *
  type ResourceValue a = a
  resourceType :: a -> Text

  resourceIdentifier :: a -> Maybe Text
  resourceIdentifier = const Nothing

  resourceLinks :: a -> Links
  resourceLinks = const mempty

  resourceMetaData :: a -> Meta
  resourceMetaData = const mempty

  resourceRelationships :: a -> Relationships
  resourceRelationships = const mempty

  fromResource :: Resource (ResourceValue a) -> a

  default fromResource :: (ResourceValue a ~ a) => Resource (ResourceValue a) -> a
  fromResource = _resValue

  toResource :: a -> Resource (ResourceValue a)

  default toResource :: (ResourceValue a ~ a) => a -> Resource (ResourceValue a)
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)

instance ResourcefulEntity (Resource a) where
  type ResourceValue (Resource a) = a
  resourceType = _datatype . _resIdentifier
  resourceIdentifier = _ident . _resIdentifier
  resourceLinks = _resLinks
  resourceRelationships = _resRelationships
  fromResource = Prelude.id
  toResource = Prelude.id

mkRelationships :: [(Text, Relationship)] -> Relationships
mkRelationships = Relationships . HM.fromList


{- |
Constructor function for creating a Relationship record

A relationship must contain either an Identifier or a Links record
-}
mkRelationship :: These (RelationshipType Identifier) Links -> Relationship
mkRelationship (This resId) = Relationship (Just resId) mempty
mkRelationship (That links) = Relationship Nothing links
mkRelationship (These resId links) = Relationship (Just resId) links

makeLenses ''Resource
