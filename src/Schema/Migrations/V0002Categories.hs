{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0002Categories
  ( module V0001'
  , CategoryT(..)
  , DemoblogDb(..)
  , migration
  ) where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001
import qualified Schema.Migrations.V0001UserAndAuthor as V0001' hiding
  ( DemoblogDb
  )

import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

--
-- === MODELS ===
--
-- CATEGORY Model
data CategoryT f = Category
  { _categoryId :: Columnar f (SqlSerial Int)
  , _categoryTitle :: Columnar f Text
  , _categoryParentId :: PrimaryKey CategoryT (Nullable f)
  } deriving (Generic, Beamable)

type Category = CategoryT Identity

type CategoryId = PrimaryKey CategoryT Identity

deriving instance Show Category

deriving instance Eq Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f
                                            (SqlSerial Int))
                            deriving (Generic, Beamable)
  primaryKey = CategoryId . _categoryId

deriving instance Show (PrimaryKey CategoryT Identity)

deriving instance Show (PrimaryKey CategoryT (Nullable Identity))

deriving instance Eq (PrimaryKey CategoryT Identity)

deriving instance Eq (PrimaryKey CategoryT (Nullable Identity))

Category (LensFor categoryId) (LensFor categoryTitle) (CategoryId (LensFor categoryParentId)) =
  tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { user :: f (TableEntity V0001.UserT)
  , author :: f (TableEntity V0001.AuthorT)
  , category :: f (TableEntity CategoryT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

--
-- === CURRENT MIGRATIONS ===
--
migration ::
     CheckedDatabaseSettings Postgres V0001.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb =
  DemoblogDb <$> preserve (V0001.user oldDb) <*> preserve (V0001.author oldDb) <*>
  createTable
    "category"
    (Category
       (field "category_id" serial)
       (field "title" (varchar (Just 511)) notNull)
       (CategoryId (field "parent_id" (maybeType serial))))