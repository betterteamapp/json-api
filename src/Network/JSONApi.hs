{- |
Entry-point module for this package.
-}
module Network.JSONApi
( D.Document
, D.docData
, D.docLinks
, D.docMeta
, D.docIncluded
, D.ErrorDocument (..)
, D.errorDoc
, D.Included
, D.getIncluded
, E.Error (..)
, R.Relationship
, R.RelationshipType(..)
, R.Resource
, I.New
, I.Existing
, R.resIdentifier
, R.resValue
, R.resLinks
, R.resRelationships
, R.Relationships
, R.ToResourcefulEntity (..)
, R.FromResourcefulEntity (..)
, I.HasId (..)
, I.IdentifierContext(..)
, I.Identifier (..)
, I.datatype
, I.ident
, I.metadata
, L.Links(..)
, M.Meta
, M.MetaObject (..)
, L.mkLinks
, R.mkRelationship
, R.mkRelationships
, D.oneDoc
, D.manyDoc
, D.composeDoc
, D.include
, D.includes
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
