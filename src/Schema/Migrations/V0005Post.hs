{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0005Post
  ( module Schema.Migrations.V0004Tag
  , PostT(..)
  , PostId
  , PrimaryKey(..)
  , DemoblogDb(..)
  , migration
  ) where

import qualified Schema.Migrations.V0004Tag as V0004
import Schema.Migrations.V0004Tag hiding (DemoblogDb(..), migration) -- to make reexport works

import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

--
-- === MODELS ===
--
-- Post Model
data PostT f = Post
  { _postId :: Columnar f (SqlSerial Int)
  , _postTitle :: Columnar f Text
  , _postContent :: Columnar f Text
  , _postAuthor :: PrimaryKey AuthorT f
  } deriving (Generic, Beamable)

type Post = PostT Identity

type PostId = PrimaryKey PostT Identity

deriving instance Show Post

deriving instance Eq Post

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = PostId . _postId

deriving instance Show (PrimaryKey PostT Identity)

deriving instance Show (PrimaryKey PostT (Nullable Identity))

deriving instance Eq (PrimaryKey PostT Identity)

deriving instance Eq (PrimaryKey PostT (Nullable Identity))

Post (LensFor postId) (LensFor postTitle) (LensFor postContent) (V0004.AuthorId (LensFor postAuthor)) =
  tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity V0004.UserT)
  , _author :: f (TableEntity V0004.AuthorT)
  , _category :: f (TableEntity V0004.CategoryT)
  , _tag :: f (TableEntity V0004.TagT)
  , _post :: f (TableEntity PostT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

--
-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0004.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (V0004._user oldDb) <*> preserve (V0004._author oldDb) <*>
  preserve (V0004._category oldDb) <*>
  preserve (V0004._tag oldDb) <*>
  createTable
    "post"
    (Post
       (field "post_id" serial)
       (field "title" (varchar (Just 255)) notNull)
       (field "content" text notNull)
       (AuthorId (field "author_id" smallint)))
