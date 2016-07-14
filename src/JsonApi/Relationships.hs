-- |
-- Description : Fetching related resources
-- Copyright   : (c) Leonid Onokhov, 2016
-- License     : BSD3
-- Maintainer  : sopvop@gmail.com
-- Stability   : experimental
--
-- = Overview
--
-- To define a relationship you need to define 'FetchRelated' instance
-- for it. Associated resources should at least define 'HasResourceId' and
-- 'IsResource' instances.
--
-- == Example model
--
-- For example we have books and authors, and want to define relationship
-- @book has one author@.
--
-- @
-- type BookId = Text
-- type AuthorId = Text
--
-- data Book = Book { bookId       :: BookId
--                  , bookTitle    :: Text
--                  , bookAuthorId :: AuthorId
--                  }
--
-- data Author = Author { authorId :: AuthorId
--                      , authorName :: Text
--                      , authorBirthday :: Maybe UTCTime
--                      }
--
-- @
--
-- == Describe how to serialise you data into JSON API resource object
--
-- We need to define 'HasResourceId' and 'IsResource' for Author type
--
-- @
-- instance 'HasResourceId' Author where
--    resourceId a = mkResourceId (authorId a) "author"
--
-- instance 'IsResource' Author where
--    resourceAttributes a = ["name" .= authorName a
--                           ,"birthday" .= authorBirthday a ]
--
-- instance 'HasResourceId' Book where
--    resourceId a = mkResourceId (bookId a) "book"
--
-- instance 'IsResource' Book where
--    resourceAttributes b = ["title" .= bookTitle b ]
--    resourceLinks b = 'links' [("self", bookUrl (bookId b))]
--
-- @
--
-- == Describe how to fetch related data
--
-- Next we need to define how to get Author from Book object.
--
-- @
-- instance 'FetchRelated' DatabaseM Book "author" Author where
--    'fetchRelated' _ mode book = case mode of
--        'FetchRelationship' -> pure (relationship, [])
--        'FetchIncluded' -> do
--          author <-  getAuthor (bookAuthorId)
--          pure (relationship, [author])
--      where
--        authorId = 'mkResourceId' (bookAuthorId book) "author"
--        relationship = 'Relationship'
--                       Nothing -- Don't have any links
--                       (Just (SingletonData authorId))
--                       Nothing -- Don't hav any meta
--
-- @
--
-- @DatabaseM@ is a monad in which we fetch our relationships.
-- Book is a @from@ part of relationshop, @"author"@ is a name identifying
-- this relationship. And Author is type of relationship target.
--
-- == Describe resource relationships themselves
--
-- @
-- type AuthorResource = 'Resource' Author \'[]
--
-- type BookResource = 'Resource' Book \'['Rel' "author" AuthorResource]
--
-- @
--
--
-- == Using it
--
-- getBookResource :: BookId -> 'Include' -> 'FieldsFilter' -> DatabaseM Document
-- getBookResource bookId whatToInclude fieldSet = do
--   book <- getBook bookId
--   'serialiseDocument' (Proxy :: Proxy BookResource) book whatToInclude fieldSet
--
--
module JsonApi.Relationships
    ( -- * Defining resources
      ResourceId
    , mkResourceId
    , HasResourceId (..)
    , IsResource (..)
    , -- ** Helper functions for defining 'IsResource' instances
      links
    , meta
    , -- * Describing relationships
      Resource
    , Rel
    , RelInclude

    , -- * Fetching related resources
      FetchMode (..)
    , FetchRelated (..)

    ,  -- * Fetching related resource from description
      SerialiseResource (..)
    , Include (..)

    , -- * Serialising documents
      serialiseDocument
    , FieldsFilter
    ) where

import           JsonApi.Internal.Api
import           JsonApi.Internal.Document
import           JsonApi.Internal.Relationships
import           JsonApi.Internal.Resource
