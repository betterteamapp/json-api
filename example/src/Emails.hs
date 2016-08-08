{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Emails
  ( Email (..)
  , mkResourceIdentifer
  , mkLinks
  ) where

import Data.Aeson.TH
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Network.URL
import Network.JSONApi
  ( Identifier (..)
  , Links
  , toLinks
  )

-- A resource associated to a User
data Email = Email
  { emailId :: Int
  , userId  :: Int
  , address :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Email)

-- helper function to build an Identifier for an Email resource
mkResourceIdentifer :: Email -> Identifier
mkResourceIdentifer email = Identifier (pack . show . emailId $ email) "Email"

-- helper function to build links for an Email resource
mkLinks :: Email -> Links
mkLinks email = toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/emails/" <> (show $ emailId email)

toURL :: String -> URL
toURL = fromJust . importURL