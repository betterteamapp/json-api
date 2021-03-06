module Users.Actions.Index
  ( usersIndex
  ) where

import Data.Maybe (fromJust)
import Servant (Handler)
import Network.JSONApi
  ( Document
  , Links
  , Meta
  , mkMeta
  )
import qualified Network.JSONApi as JSONApi
import Network.URL
import Users

-- A 'controller' action handler
usersIndex :: Handler (Document User)
usersIndex =
  return $ indexDocument users indexLinks (indexMetaData users)
  where
    users =
      [ User 1 "Isaac" "Newton"
      , User 2 "Albert" "Einstein"
      ]

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = JSONApi.mkLinks [ ("self", selfLink) ]
  where
    selfLink = toURL "/users"

-- Builds the Meta data for the 'index' action
indexMetaData :: [a] -> Meta
indexMetaData usrs = mkMeta (UserMetaData $ length usrs)

-- Builds the repsonse Document for the 'index' action
indexDocument :: [User] -> Links -> Meta -> Document User
indexDocument users links meta =
  JSONApi.mkDocument
    users
    (Just links)
    (Just meta)

toURL :: String -> URL
toURL = fromJust . importURL
