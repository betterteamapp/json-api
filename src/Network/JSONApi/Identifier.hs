{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Identifier
( HasIdentifier (..)
, Identifier (..)
, datatype
, ident
, metadata
) where

import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.JSONApi.Meta (Meta(..))
import Prelude hiding (id)

{- |
Identifiers are used to encapsulate the minimum amount of information
to uniquely identify a resource.

This object will be found at multiple levels of the JSON-API structure

Specification: <http://jsonapi.org/format/#document-resource-identifier-objects>
-}
data Identifier = Identifier
  { _ident :: Maybe Text
  , _datatype :: Text
  , _metadata :: Meta
  } deriving (Show, Eq)

instance ToJSON Identifier where
  toJSON (Identifier resId resType resMetaData) = AE.object $ addOptional
    [ "id"            .= resId
    , "type"          .= resType
    ]
    where
      addOptional l = if HM.null (fromMeta resMetaData)
        then l
        else (("meta" .= resMetaData) : l)

instance FromJSON Identifier where
  parseJSON = AE.withObject "resourceIdentifier" $ \v -> do
    id    <- v .: "id"
    typ   <- v .: "type"
    meta  <- v .:? "meta"
    return $ Identifier id typ (fromMaybe mempty meta)


{- |
Typeclass indicating how to access an 'Identifier' for
a given datatype
-}
class HasIdentifier a where
  identifier :: a -> Identifier

makeLenses ''Identifier
