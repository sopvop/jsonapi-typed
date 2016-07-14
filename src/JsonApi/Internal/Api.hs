{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module JsonApi.Internal.Api
  ( Resource
  , Rel
  , RelInclude
  ) where

import           GHC.TypeLits (Symbol)

-- | A resource description, first argument is a resource type,
-- second argument is a list of relationships.
--
-- @
-- type AuthorResource = Resource Author '[]
-- type ReviewResource = Resource Review '[]
--
-- type BookResource = Resource Book '[ Rel "reviews" ReviewResource ]
--                                    , RelInclude "author" AuthorResource ]
-- @
--
data Resource a (relationships :: [*])

-- | Describes a relationship @name@ to another 'Resource'
data Rel (name::Symbol) a

-- | Same as 'Rel', but related resource is included in response by default,
-- unless @include@ parameter is passed to server
data RelInclude (name :: Symbol) a
