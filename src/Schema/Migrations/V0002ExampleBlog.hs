{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0002ExampleBlog
  ( module Schema.Migrations.V0001ExampleBlog
  , module Schema.Migrations.V0002ExampleBlog
  ) where

import qualified Schema.Migrations.V0001ExampleBlog as V0001 hiding
  ( PrimaryKey(UserId)
  )
import Schema.Migrations.V0001ExampleBlog hiding
  ( DemoblogDb(..)
  , PrimaryKey(UserId)
  , User
  , UserId
  , UserT(..)
  , migration
  )

import Control.Arrow
import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

data UserT f = User
  { _userId :: Columnar f (SqlSerial Int)
  , _userName :: Columnar f Text
  , _userCreatedAt :: Columnar f LocalTime
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _post :: f (TableEntity PostT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

migration ::
     CheckedDatabaseSettings Postgres V0001.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb = DemoblogDb <$> alterUserTable <*> preserve (V0001._post oldDb)
  where
    alterUserTable = alterTable (V0001._user oldDb) tableMigration
    tableMigration oldTable =
      User (V0001._userId oldTable) (V0001._userName oldTable) <$>
      addColumn (field "created_at" timestamptz (defaultTo_ now_) notNull)

db :: DatabaseSettings Postgres DemoblogDb
db = unCheckDatabase (evaluateDatabase migrations)
  where
    migrations ::
         MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres DemoblogDb)
    migrations =
      migrationStep "Add user and post tables" V0001.migration >>>
      migrationStep "Add field created_at to user table" migration

createPost content userId =
  BeamExtensions.runInsertReturningList (_post db) $
  insertExpressions
    [Post default_ (val_ content) (UserId $ fromIntegral userId)]
-- Couldn't match type ‘UserT’
--                 with ‘V0001.UserT’
-- NB: ‘V0001.UserT’ is defined at
--       /private/var/folders/hl/7t2d8v_d1yj8m47m11_58lx40000gn/T/ghc-mod44393/V0001ExampleBlog44392-39.hs:(18,1)-(21,32)
--     ‘UserT’ is defined at
--       /private/var/folders/hl/7t2d8v_d1yj8m47m11_58lx40000gn/T/ghc-mod44393/V0002ExampleBlog44392-219.hs:(35,1)-(39,32)
-- Expected type: PrimaryKey
--                   V0001.UserT
--                   (QExpr Database.Beam.Postgres.Syntax.PgExpressionSyntax s')
--   Actual type: PrimaryKey
--                   UserT (QExpr Database.Beam.Postgres.Syntax.PgExpressionSyntax s')
