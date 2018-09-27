{- |
Module representing a JSON-API link object.

Specification: <http://jsonapi.org/format/#document-links>
-}
module Network.JSONApi.Link
  ( Links(..)
  , Rel
  , Href
  , mkLinks
  , buildLink
  , unsafeLinks
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable
import Data.Semigroup
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified GHC.Generics as G
import URI.ByteString (URIRef, serializeURIRef')

{- |
Type representing a JSON-API link object.

Links are an abstraction around an underlying Map consisting of
relevance identifiers as keys and URIs as values.

Example JSON:
@
  "links": {
    "self": "http://example.com/posts/1"
  }
@

Specification: <http://jsonapi.org/format/#document-links>
-}
newtype Links = Links { fromLinks :: HM.HashMap Rel Href }
  deriving (Show, Eq, ToJSON, FromJSON, G.Generic, Semigroup, Monoid, Hashable)

type Rel = Text
type Href = Text

{- |
Constructor function for building Links
-}
mkLinks :: [(Rel, URIRef a)] -> Links
mkLinks =  unsafeLinks . map buildLink

buildLink :: (Rel, URIRef a) -> (Rel, Href)
buildLink (key, url) = (key, decodeUtf8 $ serializeURIRef' url)

unsafeLinks :: [(Rel, Text)] -> Links
unsafeLinks = Links . HM.fromList
