module TestHelpers where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (fromRight)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.JSONApi
import Network.JSONApi.Resource
import Network.JSONApi.Identifier
import URI.ByteString (URIRef, Absolute, parseURI, strictURIParserOptions)
import URI.ByteString.QQ (uri, relativeRef)

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.defConfig { AE.confIndent = AE.Spaces 2, AE.confCompare = mempty }

class HasIdentifiers a where
  uniqueId :: a -> Int
  typeDescriptor :: a -> Text

data TestResource = TestResource
  { myId :: Int
  , myName :: Text
  , myAge :: Int
  , myFavoriteFood :: Text
  } deriving (Show, Generic)

instance AE.ToJSON TestResource
instance AE.FromJSON TestResource
instance Applicative m => ToResourcefulEntity m TestResource where
instance HasId TestResource where
  resourceId = pack . show . myId
instance IdentifierContext TestResource where
  resourceType _ = "testResource"

data OtherTestResource = OtherTestResource
  { myFavoriteNumber :: Int
  , myJob :: Text
  , myPay :: Int
  , myEmployer :: Text
  } deriving (Show, Generic)

instance AE.ToJSON OtherTestResource
instance AE.FromJSON OtherTestResource
instance Applicative m => ToResourcefulEntity m OtherTestResource
instance HasId OtherTestResource where
  resourceId = pack . show . myFavoriteNumber
instance IdentifierContext OtherTestResource where
  resourceType _ = "otherTestResource"

data TestMetaObject = TestMetaObject
  { totalPages :: Int
  , isSuperFun :: Bool
  } deriving (Show, Generic)

instance AE.ToJSON TestMetaObject
instance AE.FromJSON TestMetaObject
instance MetaObject TestMetaObject where
  typeName _ = "importantData"

toResource' :: (HasId a, IdentifierContext a) => a
            -> Links
            -> Meta
            -> Resource Existing a
toResource' obj links meta =
  Resource
    ((existing obj) { _metadata = meta })
    obj
    links
    mempty

linksObj :: Links
linksObj = mkLinks [ ("self", [relativeRef|/things/1|])
                   ]
           <>
           mkLinks [ ("related", [uri|http://some.domain.com/other/things/1|])
                   ]

testObject :: TestResource
testObject = TestResource 1 "Fred Armisen" 51 "Pizza"

testObject2 :: TestResource
testObject2 = TestResource 2 "Carrie Brownstein" 35 "Lunch"

otherTestObject :: OtherTestResource
otherTestObject = OtherTestResource 999 "Atom Smasher" 100 "Atom Smashers, Inc"

testMetaObj :: Meta
testMetaObj = mkMeta (TestMetaObject 3 True)

emptyMeta :: Maybe Meta
emptyMeta = Nothing

toURL :: String -> URIRef Absolute
toURL s = fromRight (error "It wasn't right") $ parseURI strictURIParserOptions (BS.toStrict $ BS.pack $ s)

emptyLinks :: Maybe Links
emptyLinks = Nothing
