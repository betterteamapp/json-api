module Network.JSONApi.ResourceSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor.Identity
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.JSONApi
import URI.ByteString.QQ (uri, relativeRef)
import TestHelpers (prettyEncode)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ runIdentity $ toResource testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (Resource Existing TestObject)
      isJust decodedJson `shouldBe` True
      {- putStrLn encodedJson -}
      {- putStrLn $ show . fromJust $ decodedJson -}

data TestObject = TestObject
  { myId :: Int
  , myName :: Text
  , myAge :: Int
  , myFavoriteFood :: Text
  } deriving (Show, Generic)

instance AE.ToJSON TestObject
instance AE.FromJSON TestObject

instance IdentifierContext TestObject where
  resourceType _ = "TestObject"

instance HasId TestObject where
  resourceId = pack . show . myId

instance (Applicative m) => ToResourcefulEntity m TestObject where
  resourceLinks _ = pure myResourceLinks
  resourceMetadata _ = pure myResourceMetaData
  resourceRelationships _ = pure myRelationshipss

data Pagination = Pagination
  { currentPage :: Int
  , totalPages :: Int
  } deriving (Show, Generic)

instance AE.ToJSON Pagination
instance AE.FromJSON Pagination
instance MetaObject Pagination where
  typeName _ = "pagination"

myRelationshipss :: Relationships
myRelationshipss =
  mkRelationships [ ("FriendOfTestObject", relationship)
                  , ("CousinOfTestObject", otherRelationship)
                  ]

relationship :: Relationship
relationship = mkRelationship $ These
  (ToOne $ pure $ Identifier "42" "TestObject" mempty)
  myResourceLinks

otherRelationship :: Relationship
otherRelationship = mkRelationship $ These
  (ToMany $ pure $ Identifier "49" "TestObject" mempty)
  myResourceLinks

myResourceLinks :: Links
myResourceLinks =
  mkLinks [ ("self", [relativeRef|/me|])
          , ("related", [relativeRef|/tacos/4|])
          ]

myResourceMetaData :: Meta
myResourceMetaData = mkMeta (Pagination 1 14)

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"
