-- |
-- Description : Combinators describing JSON API resources
-- Copyright   : (c) Leonid Onokhov, 2016
-- License     : BSD3
-- Maintainer  : sopvop@gmail.com
-- Stability   : experimental
--
-- Combinators describing JSON API resources

module JsonApi.Api
    ( -- * Resources
      Resource
    , -- * Relationships
      Rel
    , RelInclude
    )
    where

import           JsonApi.Internal.Api
