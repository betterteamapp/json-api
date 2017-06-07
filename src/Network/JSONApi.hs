{- |
Entry-point module for this package.
-}
module Network.JSONApi
( D.Document
, D.docData
, D.docLinks
, D.docMeta
, D.docIncluded
, D.ResourceData (..)
, D.ErrorDocument (..)
, D.Included
, E.Error (..)
, R.Relationship
, R.RelationshipType(..)
, R.Resource (..)
, R.resIdentifier
, R.resValue
, R.resLinks
, R.resRelationships
, R.Relationships
, R.ResourcefulEntity (..)
, I.HasIdentifier (..)
, I.Identifier (..)
, L.Links(..)
, M.Meta
, M.MetaObject (..)
, L.mkLinks
, R.mkRelationship
, R.mkRelationships
, D.mkDocument
, D.mkDocument'
, D.singleton
, D.list
, D.mkCompoundDocument
, D.mkCompoundDocument'
, D.mkIncludedResource
, M.mkMeta
, M.rawMeta
, module These
) where

import           Data.These as These
import qualified Network.JSONApi.Error as E
import qualified Network.JSONApi.Document as D
import qualified Network.JSONApi.Identifier as I
import qualified Network.JSONApi.Link as L
import qualified Network.JSONApi.Meta as M
import qualified Network.JSONApi.Resource as R

