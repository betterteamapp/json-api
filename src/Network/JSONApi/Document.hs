{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi.Document
  ( Document
  , docData
  , docLinks
  , docMeta
  , docIncluded
  , AnyData (..)
  , Single(..)
  , ErrorDocument (..)
  , errorDoc
  , Included
  , getIncluded
  , oneDoc
  , manyDocs
  , unsafeDoc
  , include
  , includes
  , singleton
  , list
  ) where

import Control.Monad (mzero)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , Value
  , (.=)
  , (.:)
  , (.:?)
  )
import Control.Lens.TH
import qualified Data.Aeson as AE
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import qualified GHC.Generics as G
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link as L
import Network.JSONApi.Meta as M
import Network.JSONApi.Resource (Resource, ResourcefulEntity)
import qualified Network.JSONApi.Resource as R

{- |
The 'Resource' type encapsulates the underlying 'Resource'

Included in the top-level 'Document', the 'Resource' may be either
a singleton resource or a list.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data AnyData a = Singleton (Resource a)
               | List [Resource a]
               deriving (Show, Eq, G.Generic1, G.Generic, Functor)

newtype Single a = Single { fromSingle :: a }
  deriving (Show, Eq, G.Generic1, G.Generic, Functor, ToJSON, FromJSON)

{- |
The 'Document' type represents the top-level JSON-API requirement.

@data@ attribute - the resulting JSON may be either a singleton resource
or a list of resources. See 'Resource' for the construction.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document f a = Document
  { _docData  ::  f (Resource a)
  , _docLinks ::  Links
  , _docMeta  ::  Meta
  , _docIncluded :: [Value]
  } deriving (G.Generic)

deriving instance (Show (f (Resource a))) => Show (Document f a)
deriving instance (Eq (f (Resource a))) => Eq (Document f a)

makeLenses ''Document

instance (ToJSON (f (Resource a))) => ToJSON (Document f a) where
  toJSON (Document vs links meta included) = AE.object
    (("data" .= vs) : optionals)
    where
      optionals = catMaybes
        [ if (HM.null $ fromLinks links) then Nothing else Just ("links" .= links)
        , if (HM.null $ fromMeta meta) then Nothing else Just ("meta"  .= meta)
        , if (null included) then Nothing else Just ("included" .= included)
        ]

instance (FromJSON (f (Resource a))) => FromJSON (Document f a) where
  parseJSON = AE.withObject "document" $ \v -> do
    d <- v .:  "data"
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
newtype Included = Included (DL.DList Value)
  deriving (Show, Monoid)

getIncluded :: Included -> [Value]
getIncluded (Included d) = DL.toList d

{- |
Constructor function for the Document data type.
-}
oneDoc :: (ToJSON (R.ResourceValue a), ResourcefulEntity a) => a -> Document Single (R.ResourceValue a)
oneDoc = unsafeDoc . Single

{- |
Constructor function for the Document data type.
-}
manyDocs :: (ToJSON (R.ResourceValue a), ResourcefulEntity a) => [a] -> Document [] (R.ResourceValue a)
manyDocs = unsafeDoc

{- |
Constructor function for the Document data type. It is possible to create an
invalid Document if the provided @f@ doesn't serialize to either single
@ResourceValue@ or an array of @ResourceValue@s
-}
unsafeDoc :: (Functor f, AE.ToJSON (f (R.ResourceValue a)), ResourcefulEntity a) => f a -> Document f (R.ResourceValue a)
unsafeDoc as = Document (R.toResource <$> as) mempty mempty mempty

{- |
Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
include :: (AE.ToJSON (R.ResourceValue a), ResourcefulEntity a) => a -> Included
include = Included . DL.singleton . AE.toJSON . R.toResource

{- |
Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
includes :: (Foldable f, AE.ToJSON (R.ResourceValue a), ResourcefulEntity a) => f a -> Included
includes = Included . DL.fromList . fmap (AE.toJSON . R.toResource) . toList

singleton :: ResourcefulEntity a => a -> AnyData (R.ResourceValue a)
singleton = Singleton . R.toResource

list :: ResourcefulEntity a => [a] -> AnyData (R.ResourceValue a)
list = List . map R.toResource

instance (ToJSON a) => ToJSON (AnyData a) where
  toJSON (Singleton res) = AE.toJSON res
  toJSON (List res)      = AE.toJSON res

instance (FromJSON a) => FromJSON (AnyData a) where
  parseJSON (AE.Object v) = Singleton <$> (AE.parseJSON (AE.Object v))
  parseJSON (AE.Array v)  = List <$> (AE.parseJSON (AE.Array v))
  parseJSON _             = mzero

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
