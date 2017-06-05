{-# LANGUAGE DeriveFunctor #-}
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import Data.These (These(..))
import GHC.Generics hiding (Meta)
import Network.JSONApi.Identifier (HasIdentifier (..), Identifier (..))
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta (Meta)
import Prelude hiding (id)

{- |
A type representing the Relationship between 2 entities

A Relationship provides basic information for fetching further information
about a related resource.

Specification: <http://jsonapi.org/format/#document-resource-object-relationships>
-}
data Relationship = Relationship
  { _data :: Maybe (RelationshipType Identifier)
  , _links :: Maybe Links
  } deriving (Show, Eq, Generic)

instance ToJSON Relationship where
  toJSON = AE.genericToJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

instance FromJSON Relationship where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }


data Relationships = Relationships (Map Text Relationship)
  deriving (Show, Eq, Generic)

instance ToJSON Relationships
instance FromJSON Relationships

instance Monoid Relationships where
  mempty = Relationships Map.empty
  mappend (Relationships a) (Relationships b) = Relationships (a <> b)

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
  , _resLinks :: Maybe Links
  , _resRelationships :: Maybe Relationships
  } deriving (Show, Eq, Generic)

makeFields ''Resource

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj linksObj rels) =
    AE.object [ "id"            .= resId
              , "type"          .= resType
              , "attributes"    .= resObj
              , "links"         .= linksObj
              , "meta"          .= metaObj
              , "relationships" .= rels
              ]

instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = AE.withObject "resourceObject" $ \v -> do
    id    <- v .:? "id"
    typ   <- v .: "type"
    attrs <- v .: "attributes"
    links <- v .:? "links"
    meta  <- v .:? "meta"
    rels  <- v .:? "relationships"
    return $ Resource (Identifier id typ meta) attrs links rels

instance HasIdentifier (Resource a) where
  identifier = _resIdentifier

{- |
A typeclass for decorating an entity with JSON API properties
-}
class ResourcefulEntity a where
  resourceIdentifier :: a -> Maybe Text
  resourceType :: a -> Text

  resourceLinks :: a -> Maybe Links
  resourceLinks = const Nothing

  resourceMetaData :: a -> Maybe Meta
  resourceMetaData = const Nothing

  resourceRelationships :: a -> Maybe Relationships
  resourceRelationships = const Nothing

  fromResource :: Resource a -> a
  fromResource = _resValue

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)

mkRelationships :: [(Text, Relationship)] -> Relationships
mkRelationships = Relationships . Map.fromList


{- |
Constructor function for creating a Relationship record

A relationship must contain either an Identifier or a Links record
-}
mkRelationship :: These (RelationshipType Identifier) Links -> Relationship
mkRelationship (This resId) = Relationship (Just resId) Nothing
mkRelationship (That links) = Relationship Nothing $ Just links
mkRelationship (These resId links) = Relationship (Just resId) (Just links)

makeLenses ''Resource
