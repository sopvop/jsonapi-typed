{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module JsonApi.Internal.Document
    ( Document (..)
    , ResourceId (..)
    , mkResourceId
    , ResourceType (..)
    , ResourceKey (..)
    , ResourceObject(..)
    , Links (..)
    , Meta (..)
    , ResourceData (..)
    , Relationship (..)
    , ResourceLinkage
    ) where

import           Data.Aeson
    (FromJSON (..), ToJSON (..), Value (..), object, pairs, (.=))
import           Data.Aeson.Types    (unsafeToEncoding)
import           Data.Hashable       (Hashable (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Monoid         ((<>))
import           Data.String         (IsString)
import           Data.Text           (Text)

-- | A top-level object of the JSON API.
--
-- <http://jsonapi.org/format/#document-top-level JSON API Spec>
data Document = Document
    { documentData     :: !(ResourceData ResourceObject)
                          -- ^ Main resource data
    , documentMeta     :: !(Maybe Meta)
                          -- ^ Optional meta data
    , documentIncluded :: ![ResourceObject]
                          -- ^ Related documents included with response
    } deriving (Show)

instance ToJSON Document where
  toJSON (Document ddata meta included) =
      object . doIncluded $ doMeta ["data" .= ddata]
    where
     doMeta o = case meta of
                  Nothing -> o
                  Just m -> "meta" .= m : o
     doIncluded o = if List.null included
                    then o
                    else "included" .= included : o

  toEncoding (Document ddata meta included) =
      pairs $ doIncluded <> doMeta <> "data" .= ddata
    where
     doMeta = case meta of
                Nothing -> mempty
                Just m -> "meta" .= m
     doIncluded = if List.null included
                  then mempty
                  else "included" .= included


-- | A unique key identifying resource among other resources of same type
newtype ResourceKey = ResourceKey { unResourceKey :: Text }
    deriving(Show, Read, Eq, Ord, Hashable, ToJSON, FromJSON)

-- | Type of the resource
newtype ResourceType = ResourceType { unResourceType :: Text }
    deriving(Show, Read, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

-- | Resource identifier object.
--
-- <http://jsonapi.org/format/#document-resource-identifier-objects JSON API Spec>
data ResourceId = ResourceId
    { resourceKey  :: {-# UNPACK #-} !ResourceKey
    , resourceType :: {-# UNPACK #-} !ResourceType
    } deriving(Show, Read, Eq, Ord)

instance ToJSON ResourceId where
  toJSON (ResourceId rid rtype) = object ["id" .= rid, "type" .= rtype]
  toEncoding (ResourceId rid rtype) = pairs ("id" .= rid <> "type" .= rtype)
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance Hashable ResourceId where
  hashWithSalt salt (ResourceId rid rtyp) =
      salt `hashWithSalt` rid `hashWithSalt` rtyp


-- | 'ResourceId' constructor helper
mkResourceId :: Text -> Text -> ResourceId
mkResourceId k t = ResourceId (ResourceKey k) (ResourceType t)
{-# INLINE CONLIKE mkResourceId #-}

-- | JSON API @data@ object. Represents primary data.
data ResourceData a = NoData  -- ^ Empty to-one relationship
                    | SingletonData !a  -- ^ To-one relationship
                    | CollectionData ![a] -- ^ To-many relationship
                   deriving (Eq, Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ResourceData a) where
  toJSON d = case d of
               NoData -> Null
               SingletonData r -> toJSON r
               CollectionData r -> toJSON r

  toEncoding d = case d of
               NoData -> unsafeToEncoding "null"
               SingletonData r -> toEncoding r
               CollectionData r -> toEncoding r
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}


-- | JSON API @resource object@
--
-- <http://jsonapi.org/format/#document-resource-objects JSON API Spec>
data ResourceObject = ResourceObject
    { resourceObjectId    :: !ResourceId
         -- ^ id and type pair
    , resourceObjectLinks :: !(Maybe Links)
        -- ^ links associated with resource
    , resourceObjectRels  :: ![(Text, Relationship)]
        -- ^ Related resources
    , resourceObjectAttrs :: ![(Text, Value)]
          -- ^ Object attributes, main data.
    , resourceObjectMeta  :: !(Maybe Meta)
          -- ^ Object meta
    } deriving (Show, Eq)

instance ToJSON ResourceObject where
  toJSON (ResourceObject rid links rels attrs meta) =
      object . doLinks . doMeta . doRels . doLinks $ doAttrs
           [ "id" .= resourceKey rid
           , "type" .= resourceType rid ]
    where
      doLinks o = case links of
                    Nothing -> o
                    Just l -> "links" .= l : o
      doRels o = if List.null rels
                 then o
                 else "relationships" .= HashMap.fromList rels : o
      doAttrs o = if List.null attrs
                  then o
                  else "attributes" .= HashMap.fromList attrs : o
      doMeta o = case meta of
                   Nothing -> o
                   Just m -> "meta" .= m : o

  toEncoding (ResourceObject rid links rels attrs meta) =
      pairs $ mconcat
           ["id" .= resourceKey rid
           ,"type" .= resourceType rid

           , fromMaybe mempty $ ("links" .=) <$> links

           , if List.null rels
             then mempty
             else "relationships" .= HashMap.fromList rels

           , if List.null attrs
             then mempty
             else "attributes" .= HashMap.fromList attrs

           , fromMaybe mempty $ ("meta" .=) <$> meta
           ]


-- | A JSON object with links associated with object
newtype Links = Links { unLinks :: HashMap Text Text }
    deriving (Show, Eq, Monoid, ToJSON, FromJSON)

-- | A JSON object holding custom meta information
--
-- <http://jsonapi.org/format/#document-meta JSON API Spec>
newtype Meta = Meta { unMeta :: HashMap Text Value }
    deriving (Show, Eq, Monoid, ToJSON, FromJSON)

-- | JSON API @relationship object@
--
-- <http://jsonapi.org/format/#document-resource-object-relationships JSON API Spec>
data Relationship = Relationship
    { relationshipLinks   :: !(Maybe Links)
                           -- ^ Associated links
    , relationshipLinkage :: !(Maybe ResourceLinkage)
         -- ^ Optional resource linkage object. Unlike 'NotData' which means
         -- there is no object set as related, 'Nothing' means data
         -- was retrieved for this resource because it was not requested.
    , relationshipMeta    :: !(Maybe Meta)
         -- ^ Optional meta data
    } deriving (Show, Eq)


instance ToJSON Relationship where
  toJSON v = object $ catMaybes
             [ ("links" .=) <$> relationshipLinks v
             , ("data"  .=) <$> relationshipLinkage v
             , ("meta"  .=) <$> relationshipMeta v
             ]
  toEncoding v = pairs $ mconcat
             [ fromMaybe mempty $ ("links" .=) <$> relationshipLinks v
             , fromMaybe mempty $ ("data"  .=) <$> relationshipLinkage v
             , fromMaybe mempty $ ("meta"  .=) <$> relationshipMeta v
             ]



-- | JSON API @resource linkage@ object
--
-- <http://jsonapi.org/format/#document-resource-object-linkage JSON API spec>
type ResourceLinkage = ResourceData ResourceId
