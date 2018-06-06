{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0007TagOfPost
  ( module Schema.Migrations.V0006Comment
  , module Schema.Migrations.V0007TagOfPost
  ) where

import qualified Schema.Migrations.V0006Comment as V0006
import Schema.Migrations.V0006Comment hiding (DemoblogDb(..), migration) -- to make reexport works

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
-- TagOfPost Model
data TagOfPostT f = TagOfPost
  { _tagOfPostId :: Columnar f (SqlSerial Int)
  , _tagOfPostPostId :: PrimaryKey PostT f
  , _tagOfPostTagId :: PrimaryKey TagT f
  } deriving (Generic, Beamable)

type TagOfPost = TagOfPostT Identity

type TagOfPostId = PrimaryKey TagOfPostT Identity

deriving instance Show TagOfPost

deriving instance Eq TagOfPost

instance Table TagOfPostT where
  data PrimaryKey TagOfPostT f = TagOfPostId (Columnar f
                                              (SqlSerial Int))
                             deriving (Generic, Beamable)
  primaryKey = TagOfPostId . _tagOfPostId

deriving instance Show (PrimaryKey TagOfPostT Identity)

deriving instance Show (PrimaryKey TagOfPostT (Nullable Identity))

deriving instance Eq (PrimaryKey TagOfPostT Identity)

deriving instance Eq (PrimaryKey TagOfPostT (Nullable Identity))

TagOfPost (LensFor tagOfPostId) (PostId (LensFor tagOfPostPostId)) (TagId (LensFor tagOfPostTagId)) =
  tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _author :: f (TableEntity AuthorT)
  , _category :: f (TableEntity CategoryT)
  , _tag :: f (TableEntity TagT)
  , _post :: f (TableEntity PostT)
  , _comment :: f (TableEntity CommentT)
  , _tagOfPost :: f (TableEntity TagOfPostT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

--
-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0006.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (V0006._user oldDb) <*> preserve (V0006._author oldDb) <*>
  preserve (V0006._category oldDb) <*>
  preserve (V0006._tag oldDb) <*>
  preserve (V0006._post oldDb) <*>
  preserve (V0006._comment oldDb) <*>
  createTable
    "tag_of_posts"
    (TagOfPost
       (field "tag_of_post_id" serial)
       (PostId (field "post_id" smallint))
       (TagId (field "tag_id" smallint)))
