{- |
Module representing a JSON-API error object.

Error objects are used for providing application-specific detail
to unsuccessful API responses.

Specification: <http://jsonapi.org/format/#error-objects>
-}
module Network.JSONApi.Error
( Error (..)
) where

import Data.Aeson (ToJSON, FromJSON, Object)
import Data.Default
import Data.Text
import qualified GHC.Generics as G
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta
import Prelude hiding (id)

{- |
Type for providing application-specific detail to unsuccessful API
responses.

Specification: <http://jsonapi.org/format/#error-objects>
-}
data Error a = Error
  { id :: Maybe Text
  -- ^ a unique identifier for this particular occurrence of the problem.
  , links :: Maybe Links
  -- ^  a links object containing the following members:
  --
  -- [@about@] a link that leads to further details about this particular occurrence of the problem.
  , status :: Maybe Text
  -- ^ the HTTP status code applicable to this problem, expressed as a string value.
  , code :: Maybe Text
  -- ^ an application-specific error code, expressed as a string value.
  , title :: Maybe Text
  -- ^ a short, human-readable summary of the problem that SHOULD NOT change from occurrence to occurrence of the problem, except for purposes of localization.
  , detail :: Maybe Text
  -- ^ a human-readable explanation specific to this occurrence of the problem. Like title, this field’s value can be localized.
  , meta :: Maybe Meta
  -- ^ a meta object containing non-standard meta-information about the error.
  , source :: Maybe Object
  -- ^ source: an object containing references to the source of the error, optionally including any of the following members:
  --
  -- [@pointer@] a JSON Pointer [RFC6901] to the associated entity in the request document [e.g. @\/data@ for a primary data object, or @\/data\/attributes\/title@ for a specific attribute].
  -- [@parameter@] a string indicating which URI query parameter caused the error.
  } deriving (Show, Eq, G.Generic)

instance ToJSON (Error a)
instance FromJSON (Error a)

instance Default (Error a) where
  def =
    Error
      { id = Nothing
      , links = Nothing
      , status = Nothing
      , code = Nothing
      , title = Nothing
      , detail = Nothing
      , meta = Nothing
      , source = Nothing
      }
