{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Control.Monad.Trans.Reader (Reader, ask, runReader)
import           Data.Aeson                 (ToJSON, (.=))
import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Function              (on)
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import qualified Data.List                  as List
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           JsonApi.Document
import           JsonApi.Relationships

newtype BookId = BookId Text
  deriving (Eq, Ord, Hashable, Show, ToJSON)

newtype ChapterId = ChapterId Int
  deriving (Eq, Ord, Hashable, Show, ToJSON)

newtype AuthorId = AuthorId Int
  deriving (Eq, Ord, Hashable, Show, ToJSON)

newtype CommentId = CommentId Int
  deriving (Eq, Ord, Hashable, Show, ToJSON)

textBookId :: BookId -> Text
textBookId (BookId i) = i
textChapterId :: ChapterId -> Text
textChapterId (ChapterId i) = Text.pack (show i)
textAuthorId :: AuthorId -> Text
textAuthorId (AuthorId i) = Text.pack (show i)
textCommentId :: CommentId -> Text
textCommentId (CommentId i) = Text.pack (show i)

instance HasResourceId BookId where
  resourceId (BookId t) =
      mkResourceId t "book"

instance HasResourceId ChapterId where
  resourceId t =
      mkResourceId (textChapterId t) "chapter"

instance HasResourceId AuthorId where
  resourceId a =
      mkResourceId (textAuthorId a) "author"

instance HasResourceId CommentId where
  resourceId c =
      mkResourceId (textCommentId c) "comment"


data Book = Book
    { bookId       :: BookId
    , bookTitle    :: Text
    , bookScore    :: Int
    , bookAuthorId :: AuthorId
    } deriving (Show)

data Chapter = Chapter
    { chapterId       :: ChapterId
    , chapterBookId   :: BookId
    , chapterOrdering :: Int
    , chapterTitle    :: Text
    } deriving (Show)

data Author = Author
    { authorId    :: AuthorId
    , authorName  :: Text
    , authorScore :: Int
    } deriving (Show)

data Comment = Comment
    { commentId     :: CommentId
    , commentBookId :: BookId
    , commentText   :: Text
    , commentTitle  :: Text
    } deriving (Show)

data BookDB = BookDB
    { bookDB           :: HashMap BookId Book
    , chapterDB        :: HashMap ChapterId Chapter
    , authorDB         :: HashMap AuthorId Author
    , commentDB        :: HashMap CommentId Comment
    , chaptersByBookDB :: HashMap BookId [ChapterId]
    , commentsByBookDB :: HashMap BookId [CommentId]
    }

testDB :: BookDB
testDB = BookDB
         (HashMap.fromList $ mkIds bookId books)
         (HashMap.fromList $ mkIds chapterId chapters)
         (HashMap.fromList $ mkIds authorId authors)
         (HashMap.fromList $ mkIds commentId comments)
         (HashMap.fromList $ mkReverse chapterBookId chapterId chapters)
         (HashMap.fromList $ mkReverse commentBookId commentId comments)
  where
    mkIds f = map (\b -> (f b, b))
    mkReverse f t = map (\c -> (f (head c), map t c))
                    . List.groupBy ((==) `on` f)
                    . List.sortOn f
    books = [ Book (BookId "isbn-1") "LYAH" 10 (AuthorId 0)
            , Book (BookId "isbn-2") "RWH" 20 (AuthorId 1)]
    chapters =
     [ Chapter (ChapterId 0) (BookId "isbn-1") 1 "Introduction"
     , Chapter (ChapterId 1) (BookId "isbn-1") 2 "Starting Out"
     , Chapter (ChapterId 2) (BookId "isbn-1") 3 "Types and Typeclasses"

     , Chapter (ChapterId 3) (BookId "isbn-2") 1 "Getting started"
     , Chapter (ChapterId 4) (BookId "isbn-2") 2 "Types and functions"
     , Chapter (ChapterId 5) (BookId "isbn-2") 3 "Defining types, streamlining functions"
     ]
    authors = [ Author (AuthorId 0) "Miran Lipovača" 20
              , Author (AuthorId 1) "Bryan O'Sullivan" 25 ]
    comments = [ Comment (CommentId 0) (BookId "isbn-1") "Great book" "I finally get monads"
               , Comment (CommentId 1) (BookId "isbn-2") "FrustrateD" "Nothing compiles" ]


instance HasResourceId Book where
  resourceId = resourceId . bookId

instance HasResourceId Author where
  resourceId = resourceId . authorId

instance HasResourceId Chapter where
  resourceId = resourceId . chapterId

instance HasResourceId Comment where
  resourceId = resourceId . commentId

instance IsResource Book where
  resourceLinks b =
      links [("self", "/books/" <> textBookId (bookId b))]
  resourceAttributes b =
      ["title" .= bookTitle b
      ,"score" .= bookScore b
      ]

instance IsResource Chapter where
  resourceLinks b =
      links [("self", "/chapter/" <> textChapterId (chapterId b))]

  resourceAttributes b =
      ["title" .= chapterTitle b
      ,"score" .= chapterOrdering b
      ]


instance IsResource Author where
  resourceLinks a =
      links [("self", "/autors/" <> textAuthorId (authorId a))]
  resourceAttributes b =
      ["name" .= authorName b
      ,"score" .= authorScore b]

instance IsResource Comment where
  resourceAttributes c =
      ["title" .= commentTitle c
      ,"text" .= commentText c ]

newtype BookM a = BookM (Reader BookDB a)
    deriving (Functor, Applicative, Monad)

askDB :: BookM BookDB
askDB = BookM ask

instance FetchRelated BookM Book "author" Author where
  fetchRelated _ mode b =
      case mode of
        FetchRelationship -> pure (mkRelation, [])
        FetchIncluded -> do
          db <- askDB
          case HashMap.lookup (bookAuthorId b) $ authorDB db of
            Nothing -> error "Author not found, halp"
            Just a -> pure (mkRelation, [a])
    where
     mkRelation =
        Relationship
        (links [("related", "/author/" <> textAuthorId (bookAuthorId b))])
        (Just $ SingletonData  (resourceId $ bookAuthorId b))
        mempty

instance FetchRelated BookM Book "chapters" Chapter where
  fetchRelated _ mode b =
      case mode of
        FetchRelationship -> pure (mkRelation Nothing, [])
        FetchIncluded -> do
          db <- askDB
          let chapters = fromMaybe [] $
                  HashMap.lookup (bookId b) (chaptersByBookDB db)
                  >>= traverse (`HashMap.lookup` chapterDB db)
          pure ( mkRelation . Just . CollectionData
                             $  map resourceId chapters
               , chapters)
    where
      mkRelation r =
         Relationship
         (links [("related", "/book/" <> textBookId (bookId b) <> "/chapters")])
         r
         mempty


instance FetchRelated BookM Book "comments" Comment where
  fetchRelated _ mode b =
      case mode of
        FetchRelationship -> pure (mkRelation Nothing, [])
        FetchIncluded -> do
          db <- askDB
          case HashMap.lookup (bookId b) (commentsByBookDB db)
               >>= traverse (`HashMap.lookup` commentDB db) of
            Nothing -> error "Omg, comments not found"
            Just comments -> pure (mkRelation (Just $ CollectionData (map resourceId comments))
                                  , comments)
    where
     mkRelation r =
        Relationship
        (links [("related", "/book/" <> textBookId (bookId b) <> "/comments")])
        r
        mempty

type AuthorR = Resource Author '[]
type CommentR = Resource Comment '[]
type ChapterR = Resource  Chapter '[]

type BookR = Resource Book '[ Rel "author" AuthorR
                            , Rel "chapters" ChapterR
                            , Rel "comments" CommentR]

runBookM :: BookDB -> BookM a -> a
runBookM db (BookM r) = runReader r db


testFilter :: HashMap ResourceType (HashSet Text)
testFilter = HashMap.fromList [("book", HashSet.fromList ["title"])
                              ,("author", HashSet.fromList ["name"])]

testInclude :: Include
testInclude = IncludeChildren $
              HashMap.fromList [("comments", Include)
                               ,("chapters", Include)
                               ,("author", Include)]

runExample :: BookId -> Include -> HashMap ResourceType (HashSet Text) -> Maybe Document
runExample bookid include fieldSet = runBookM testDB $ do
  db <- askDB
  case HashMap.lookup bookid (bookDB db) of
    Nothing -> pure Nothing
    Just b -> do
        doc <- serialiseDocument (Proxy :: Proxy BookR) (SingletonData b) include fieldSet
        pure (Just doc)

main :: IO ()
main = do
  let r = runExample (BookId "isbn-1") testInclude testFilter
  BL.putStrLn $ J.encode r
