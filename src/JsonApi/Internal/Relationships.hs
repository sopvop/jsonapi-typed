{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module JsonApi.Internal.Relationships
    where

import           Data.Foldable             (foldl')
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (Endo (..), (<>))
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Traversable          (mapAccumL)
import           GHC.TypeLits

import           JsonApi.Internal.Api
import           JsonApi.Internal.Document
import           JsonApi.Internal.Resource


-- | Describes if related resources should be fetcher, or we are interested
-- in relationship itself only
data FetchMode = FetchIncluded   -- ^ Fetch relationship with related resource
               | FetchRelationship  -- ^ Fetch only relationship itself
                 deriving (Eq, Ord, Show)


-- | Class for fetching data for relationship @name@, from entity @r@
-- to entity @a@.
--
-- In 'FetchRelationship' mode, if you don't have any 'ResourceId's for
-- related resources inferable just from source resource, you should
-- return 'Nothing' in 'relationshipData' field.
--
-- In 'FetchIncluded' mode, if you have returned any 'ResourceId's in
-- 'relationshipData' field, you must also return all resources whith same
-- 'ResourceId' in second field of result pair.
--
-- @
-- instance Monad m => 'FetchRelated' Book "author" Author where
--   fetchRelated _ mode book = case mode of
--       'FetchRelationship' -> pure (bookToAuthor, [])
--       'FetchIncluded' -> do
--         author <- fetchAuthor (bookAuthorId book)
--         pure (bookToAuthor, [author])
--      where
--        bookToAuthor = 'Relationship'
--          (links [("related", authorUrl (bookAuthorid book))]
--          (Just $ 'SingletonData' (bookAuthorId book))
--          'Nothing'
-- @
--

class Monad m => FetchRelated m r (nm::Symbol) a where
  fetchRelated :: forall proxy . proxy (Rel nm a) -- ^ Relationship descriptor
               -> FetchMode -- ^ Should associated resources be fetched
               -> r         -- ^ base resource for this relationship
               -> m (Relationship, [a])
                     -- ^ Relationship and included objects, if requested.



-- | Describes what to do with related resources
data Include = IncludeDefault  -- ^ Only include those which are included by
                               -- default, like specified with 'RelInclude'
             | Include         -- ^ Include this, but not kids
             | IncludeNot      -- ^ Do not include this
             | IncludeChildren (HashMap Text Include)
                               -- ^ Include this and specified kids
             deriving (Eq, Show)

includesForKid :: Text -> Include -> Include
includesForKid _ IncludeDefault = IncludeDefault
includesForKid _ Include =  IncludeNot
includesForKid _ IncludeNot = IncludeNot
includesForKid nm (IncludeChildren hm) = fromMaybe IncludeNot $ HashMap.lookup nm hm

-- | Resources to be included in top-level document
data Included = Included
    { includedIds     :: !(HashSet ResourceId)
        -- ^ Ids of already included object
    , includedObjects :: HashMap ResourceType [ResourceObject]
    } deriving (Show)

emptyIncluded :: Included
emptyIncluded = Included mempty mempty

addIncluded  :: ResourceObject
             -> Included
             -> Included
addIncluded o s@(Included ids includedObjs) =
    if HashSet.member rid ids
    then s
    else let !r = Included
                  (HashSet.insert rid ids)
                  (HashMap.insertWith merge (resourceType rid) [o] includedObjs)
         in r
  where
    !rid = resourceObjectId o
    merge _ old = o:old

foldIncluded :: Foldable t
             => t ResourceObject
             -> Included
             -> Included
foldIncluded fs !s = foldl' (flip addIncluded) s fs


-- | fetches one of the specified relationships
class Monad m => FetchRelationship m r a where
  fetchRelationship :: forall proxy . proxy a
                    -> Include
                    -> r
                    -> m ((Text, Relationship), Endo Included)

doFetchRelationship :: ( FetchRelated m r nm a
                       , SerialiseResource m (Resource a relationships))
    => forall proxy . proxy (Rel nm a)
    -> proxy (Resource a relationships)
    -> FetchMode
    -> Include
    -> r
    -> m (Relationship, Endo Included)
doFetchRelationship thisProxy kidsProxy fetchMode includeMode a = do
    (r, includes) <- fetchRelated thisProxy fetchMode a
    related <- traverse (serialise kidsProxy includeMode) includes
    let goRel (rel, relIncs) = relIncs <> Endo (addIncluded rel)
        doIncludes = foldMap goRel related
    pure (r, doIncludes)
    where


instance ( KnownSymbol nm
         , FetchRelated m r nm a
         , SerialiseResource m (Resource a relationships) )
    => FetchRelationship m r (Rel nm (Resource a relationships)) where
  fetchRelationship _ env a = do
    (r, includes) <- doFetchRelationship
                     (Proxy :: Proxy (Rel nm a))
                     (Proxy :: Proxy (Resource a relationships))
                     fetchMode
                     includeMode
                     a
    pure ((textName, r), includes)

    where
      textName = Text.pack $ symbolVal (Proxy :: Proxy nm)
      includeMode = includesForKid textName env
      -- Only include if directly requested
      fetchMode = if includeMode == IncludeNot
                     || includeMode == IncludeDefault
                  then FetchRelationship
                  else FetchIncluded

instance ( KnownSymbol nm
         , FetchRelated m r nm a
         , related ~ Resource a relationships
         , SerialiseResource m (Resource a relationships))
    => FetchRelationship m r (RelInclude nm (Resource a relationships)) where
  fetchRelationship _ env a = do
    (r, includes) <- doFetchRelationship
                     (Proxy :: Proxy (Rel nm a))
                     (Proxy :: Proxy (Resource a relationships))
                     fetchMode
                     includeMode
                     a
    pure ((textName, r), includes)
    where
      textName = Text.pack $ symbolVal (Proxy :: Proxy nm)
      includeMode = includesForKid textName env
      -- Include by default
      fetchMode = if includeMode /= IncludeNot
                  then FetchIncluded
                  else FetchRelationship


class FetchAllRelationships m r a where
  fetchAllRelationships :: forall proxy . proxy a
                        -> Include
                        -> r
                        -> m ([(Text, Relationship)], Endo Included)


instance Applicative m => FetchAllRelationships m r '[] where
  fetchAllRelationships _ _ _ = pure mempty

instance ( Applicative m
         , FetchRelationship m r a
         , FetchAllRelationships m r as
         ) => FetchAllRelationships m r (a ': as) where
  fetchAllRelationships _ env resource =
     (\(r, incs) (rs, oincs) -> (r:rs, incs <> oincs))
     <$> fetchRelationship (Proxy :: Proxy a) env resource
     <*> fetchAllRelationships (Proxy :: Proxy as) env resource


-- | Fetches all related resources of some 'Resource'
class Monad m => SerialiseResource m a where
  type SerialiseI m a
  serialise :: forall proxy. proxy a
            -> Include
            -> SerialiseI m a
            -> m (ResourceObject, Endo Included)


instance ( Monad m
         , IsResource a
         , FetchAllRelationships m a relationships
         )
    => SerialiseResource m (Resource a relationships) where
  type SerialiseI m (Resource a relationships) = a
  serialise _ env a =
     (\(rels, includedObjs) -> (toResourceObject rels a, includedObjs)
     ) <$> fetchAllRelationships (Proxy :: Proxy relationships) env a


filterFields :: HashSet Text -> ResourceObject -> ResourceObject
filterFields keys s =
  s { resourceObjectAttrs =
        filter (flip HashSet.member keys . fst) $ resourceObjectAttrs s
    , resourceObjectRels =
        filter (flip HashSet.member keys . fst) $ resourceObjectRels s
    }

-- | Which fields to include in output by 'ResourceType'
-- used for sparse fieldsets functionality.
--
-- <http://jsonapi.org/format/#fetching-sparse-fieldsets JSON API Spec>
type FieldsFilter = HashMap ResourceType (HashSet Text)

-- | serialise resource @a@ associated with type @'SerialiseI m a'@
-- into a document. You can add links and meta to that document later
--
-- @
-- type BookResource = Resource Book '[Rel "author" (Resource Author '[])]
--
-- getBookResource :: BookId -> Document
-- getBookResource bookId = withTransaction $ do
--   book <- getBook bookId
--   'serialiseDocument' (Proxy :: Proxy BookResource)
--                      ('SingletonData' book)
--                      'IncludeDefault'
--                      mempty
--
-- @
--
serialiseDocument :: SerialiseResource m a
                  => proxy a
                  -> ResourceData (SerialiseI m a)
                  -> Include
                  -> FieldsFilter
                  -> m Document
serialiseDocument proxy a include fieldSet = do
  res <- traverse (serialise proxy include) a
  let (included, resData) = mapAccumL serialiseOne mempty res
      Included _ includedObjs = appEndo included (Included mempty mempty)
      includedObjs' =
        HashMap.foldlWithKey' filterAccumIncluded id includedObjs []
  pure $ Document resData mempty includedObjs'

  where
    serialiseOne acc (o, included) = (included <> acc, filterObject o)
    filterObject o =
        case HashMap.lookup (resourceType $ resourceObjectId o) fieldSet of
          Nothing -> o
          Just keys -> filterFields keys o

    filterAccumIncluded acc k objs =
        case HashMap.lookup k fieldSet of
          Nothing -> acc . (objs ++)
          Just keys -> acc . (map (filterFields keys) objs ++)

