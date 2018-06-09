{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0001ExampleBlog where

import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

data UserT f = User
  { _userId :: Columnar f (SqlSerial Int)
  , _userName :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data PostT f = Post
  { _postId :: Columnar f (SqlSerial Int)
  , _postContent :: Columnar f Text
  , _postAuthor :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = PostId . _postId

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _post :: f (TableEntity PostT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

migration ::
     ()
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration () =
  DemoblogDb <$>
  createTable
    "user"
    (User (field "user_id" serial) (field "name" (varchar (Just 255)) notNull)) <*>
  createTable
    "post"
    (Post
       (field "post_id" serial)
       (field "content" text notNull)
       (UserId (field "user_id" smallint)))
