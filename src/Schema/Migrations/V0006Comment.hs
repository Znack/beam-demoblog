{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0006Comment
  ( module Schema.Migrations.V0005Post
  , module Schema.Migrations.V0006Comment
  ) where

import qualified Schema.Migrations.V0005Post as V0005
import Schema.Migrations.V0005Post hiding
  ( DemoblogDb(..)
  , author
  , category
  , comment -- to make reexport works
  , migration
  , post
  , tag
  , user
  )

import Control.Lens
import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

--
-- === MODELS ===
--
-- Comment Model
data CommentT f = Comment
  { _commentId :: Columnar f (SqlSerial Int)
  , _commentContent :: Columnar f Text
  , _commentAuthor :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type Comment = CommentT Identity

type CommentId = PrimaryKey CommentT Identity

deriving instance Show Comment

deriving instance Eq Comment

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f (SqlSerial Int))
                           deriving (Generic, Beamable)
  primaryKey = CommentId . _commentId

deriving instance Show (PrimaryKey CommentT Identity)

deriving instance Show (PrimaryKey CommentT (Nullable Identity))

deriving instance Eq (PrimaryKey CommentT Identity)

deriving instance Eq (PrimaryKey CommentT (Nullable Identity))

Comment (LensFor commentId) (LensFor commentContent) (V0005.UserId (LensFor commentAuthor)) =
  tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity V0005.UserT)
  , _author :: f (TableEntity V0005.AuthorT)
  , _category :: f (TableEntity V0005.CategoryT)
  , _tag :: f (TableEntity V0005.TagT)
  , _post :: f (TableEntity V0005.PostT)
  , _comment :: f (TableEntity CommentT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

DemoblogDb (TableLens user) (TableLens author) (TableLens category) (TableLens tag) (TableLens post) (TableLens comment) =
  dbLenses
  --

-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0005.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (oldDb ^. V0005.user) <*>
  preserve (oldDb ^. V0005.author) <*>
  preserve (oldDb ^. V0005.category) <*>
  preserve (oldDb ^. V0005.tag) <*>
  preserve (oldDb ^. V0005.post) <*>
  createTable
    "comment"
    (Comment
       (field "comment_id" serial)
       (field "content" text notNull)
       (UserId (field "author_id" smallint)))
