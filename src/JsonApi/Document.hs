-- |
-- Description : Document structure
-- Copyright   : (c) Leonid Onokhov, 2016
-- License     : BSD3
-- Maintainer  : sopvop@gmail.com
-- Stability   : experimental
--
-- This module describes JSON API document structure
--
module JsonApi.Document
    ( -- * Top-level object
      Document (..)
    -- * Resource identifier object
    , ResourceId(..)
    , mkResourceId
    , ResourceKey
    , ResourceType
    -- * Resource object
    , ResourceData (..)
    , ResourceObject (..)
    -- * Relatiomships
    , Relationship (..)
    , ResourceLinkage
    , -- * Meta data
      Links (..)
    , Meta (..)
    ) where

import           JsonApi.Internal.Document
