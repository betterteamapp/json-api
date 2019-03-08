module Network.JSONApi.DocumentSpec where

import Control.Lens
import Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Lens as Lens
import Data.ByteString.Lazy.Char8 (ByteString)
{- import qualified Data.ByteString.Lazy.Char8 as BS -}
import Data.Either (isRight)
import Data.Maybe
import Data.Monoid
import Network.JSONApi
import Network.JSONApi.Document
import TestHelpers
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $ do
    it "JSON encodes/decodes a singleton resource" $ do
      -- TODO: test the main resource actually is a singleton
      let jsonApiObj = runIdentity $ oneDoc testObject
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = (decodeDocumentObject encodedJson) :: Either String (Document Existing Identity TestResource)
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "JSON encodes/decodes a list of resources" $ do
      -- TODO: test the main resource actually is a list
      let jsonApiObj = runIdentity $ manyDoc [testObject, testObject2]
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = (decodeDocumentObject encodedJson) :: Either String (Document Existing [] TestResource)
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn (show decodedJson) -}
      isRight decodedJson `shouldBe` True

    it "only contains the mandatory data field by default" $ do
      let jsonApiObj = runIdentity $ oneDoc testObject
      let encodedJson = encodeDocumentObject jsonApiObj
      let dataObject = encodedJson ^? Lens.key "data"
      let linksObject = encodedJson ^? Lens.key "links"
      let metaObject = encodedJson ^? Lens.key "meta"
      let includedObject = encodedJson ^? Lens.key "included"
      isJust dataObject `shouldBe` True
      isJust linksObject `shouldBe` False
      isJust metaObject `shouldBe` False
      isJust includedObject `shouldBe` False

    it "allows an optional top-level links object" $ do
      let jsonApiObj = runIdentity $ do
            doc <- oneDoc testObject
            pure $ doc & docLinks <>~ linksObj
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson :: Either String (Document Existing Identity TestResource)
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = runIdentity $ do
            doc <- oneDoc testObject
            pure $ doc & docMeta <>~ testMetaObj
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson :: Either String (Document Existing Identity TestResource)
      -- putStrLn (BS.unpack encodedJson)
      -- putStrLn $ show decodedJson
      isRight decodedJson `shouldBe` True

    it "allows a heterogeneous list of related resources" $ do
      let jsonApiObj = runIdentity $ do
            doc <- oneDoc testObject
            includedResources <- mconcat <$> sequence [include testObject, include testObject2]
            pure $ doc & docIncluded .~ getIncluded includedResources
      let encodedJson = encodeDocumentObject jsonApiObj
      let decodedJson = decodeDocumentObject encodedJson :: Either String (Document Existing Identity TestResource)
      {- putStrLn (BS.unpack encodedJson) -}
      {- putStrLn $ show decodedJson -}
      isRight decodedJson `shouldBe` True

    it "supports sparsifying document resources" $ do
      let sfs = queryParamSparseFields [("fields[testResource]", "myName,myAge")]
      let json = encodeDocumentObject $ makeSparseDocument sfs $ runIdentity $ oneDoc testObject
      (json ^? Lens.key "data" . Lens.key "attributes" . Lens.key "myFavoriteFood") `shouldBe` Nothing
      (json ^? Lens.key "data" . Lens.key "attributes" . Lens.key "myName" . Lens._String) `shouldBe` (Just "Fred Armisen")
      (json ^? Lens.key "data" . Lens.key "attributes" . Lens.key "myAge" . Lens._Number) `shouldBe` (Just 51)


decodeDocumentObject :: (AE.FromJSON1 f) => ByteString
                    -> Either String (Document Existing f TestResource)
decodeDocumentObject = AE.eitherDecode

encodeDocumentObject :: (AE.ToJSON1 f, ToJSON a) => Document Existing f a -> ByteString
encodeDocumentObject = prettyEncode
