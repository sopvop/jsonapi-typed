module JsonApi.Internal.Resource
    ( HasResourceId (..)
    , IsResource (..)
    , toResourceObject
    , links
    , meta
    )
    where

import           Data.Aeson                (Value)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Text                 (Text)
import           JsonApi.Internal.Document
    (Links (..), Meta (..), Relationship, ResourceId, ResourceObject (..))

-- | Object can be uniquely identified in api
class HasResourceId a where
  resourceId :: a -> ResourceId  -- ^ Get a 'ResourceId' from object

-- | Implementing object is a JSON API resource
class HasResourceId a => IsResource a where
  resourceAttributes :: a -> [(Text, Value)]
    -- ^ Content of @attributes@ key

  resourceAttributes _ = mempty
  resourceLinks :: a -> Maybe Links
    -- ^ Content of @links@ key
  resourceLinks _ = mempty
  resourceMeta  :: a -> Maybe Meta
    -- ^ Content of @meta@ key
  resourceMeta _ = mempty

-- | Helper constructor
toResourceObject :: IsResource a => [(Text, Relationship)] -> a -> ResourceObject
toResourceObject rels a = ResourceObject (resourceId a)
                                    (resourceLinks a)
                                    rels
                                    (resourceAttributes a)
                                    (resourceMeta a)
{-# INLINE CONLIKE toResourceObject #-}

-- | Helper for defining 'resourceLinks' method
--
-- @
-- instance IsResource Book where
--   resourceLinks book = links [("self", bookUrl (bookId book))]
--
-- @
links :: [(Text, Text)] -> Maybe Links
links [] = Nothing
links xs = Just . Links $ HashMap.fromList xs

-- | Helper for defining 'resourceMeta' method
--
-- @
-- instance IsResource BookSearch where
--   resourceMeta bs = meta ["count" .= length (foundBooks bs)]
--
-- @

meta :: [(Text, Value)] -> Maybe Meta
meta [] = Nothing
meta xs = Just . Meta $ HashMap.fromList xs
