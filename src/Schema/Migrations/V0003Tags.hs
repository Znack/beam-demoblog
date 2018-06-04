{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0003Tags
  ( module V0002'
  , TagT(..)
  , DemoblogDb(..)
  , migration
  ) where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001
import qualified Schema.Migrations.V0002Categories as V0002
import qualified Schema.Migrations.V0002Categories as V0002' hiding (DemoblogDb)

import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

--
-- === MODELS ===
--
-- Tag Model
data TagT f = Tag
  { _tagId :: Columnar f (SqlSerial Int)
  , _tagTitle :: Columnar f Text
  } deriving (Generic, Beamable)

type Tag = TagT Identity

type TagId = PrimaryKey TagT Identity

deriving instance Show Tag

deriving instance Eq Tag

instance Table TagT where
  data PrimaryKey TagT f = TagId (Columnar f (SqlSerial Int))
                       deriving (Generic, Beamable)
  primaryKey = TagId . _tagId

deriving instance Show (PrimaryKey TagT Identity)

deriving instance Show (PrimaryKey TagT (Nullable Identity))

deriving instance Eq (PrimaryKey TagT Identity)

deriving instance Eq (PrimaryKey TagT (Nullable Identity))

Tag (LensFor tagId) (LensFor tagTitle) = tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { user :: f (TableEntity V0001.UserT)
  , author :: f (TableEntity V0001.AuthorT)
  , category :: f (TableEntity V0002.CategoryT)
  , tag :: f (TableEntity TagT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

--
-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0002.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (V0002.user oldDb) <*> preserve (V0002.author oldDb) <*>
  preserve (V0002.category oldDb) <*>
  createTable
    "tag"
    (Tag (field "tag_id" serial) (field "title" (varchar (Just 255)) notNull))
