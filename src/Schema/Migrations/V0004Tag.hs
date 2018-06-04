{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0004Tag
  ( module Schema.Migrations.V0003Category
  , TagT(..)
  , TagId
  , PrimaryKey(..)
  , DemoblogDb(..)
  , migration
  ) where

import qualified Schema.Migrations.V0003Category as V0003
import Schema.Migrations.V0003Category hiding (DemoblogDb(..), migration) -- to make reexport works

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
  { user :: f (TableEntity V0003.UserT)
  , author :: f (TableEntity V0003.AuthorT)
  , category :: f (TableEntity V0003.CategoryT)
  , tag :: f (TableEntity TagT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

--
-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0003.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (V0003.user oldDb) <*> preserve (V0003.author oldDb) <*>
  preserve (V0003.category oldDb) <*>
  createTable
    "tag"
    (Tag (field "tag_id" serial) (field "title" (varchar (Just 255)) notNull))
