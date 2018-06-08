module Schema.Database
  ( module Schema.Migrations.V0007TagOfPost
  , migration
  , checkedDb
  , db
  , TableSelector
  ) where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001 (migration)
import qualified Schema.Migrations.V0002UserTableIsAdmin as V0002 (migration)
import qualified Schema.Migrations.V0003Category as V0003 (migration)
import qualified Schema.Migrations.V0004Tag as V0004 (migration)
import qualified Schema.Migrations.V0005Post as V0005 (migration)
import qualified Schema.Migrations.V0006Comment as V0006 (migration)
import qualified Schema.Migrations.V0007TagOfPost as V0007 (migration)

import Schema.Migrations.V0007TagOfPost hiding (migration)

import Control.Arrow

import Database.Beam
import Database.Beam.Migrate.Types
  ( CheckedDatabaseSettings
  , MigrationSteps
  , evaluateDatabase
  , migrationStep
  , unCheckDatabase
  )
import Database.Beam.Postgres (PgCommandSyntax, Postgres)

migration ::
     MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres DemoblogDb)
migration =
  migrationStep "Add user and author tables" V0001.migration >>>
  migrationStep "Add field isAdmin to user table" V0002.migration >>>
  migrationStep "Add category table" V0003.migration >>>
  migrationStep "Add tag table" V0004.migration >>>
  migrationStep "Add post table" V0005.migration >>>
  migrationStep "Add comment table" V0006.migration >>>
  migrationStep "Add tagOfPost table" V0007.migration

checkedDb :: CheckedDatabaseSettings Postgres DemoblogDb
checkedDb = evaluateDatabase migration

db :: DatabaseSettings Postgres DemoblogDb
db = unCheckDatabase checkedDb

DemoblogDb (TableLens user) (TableLens author) (TableLens category) (TableLens tag) (TableLens post) (TableLens comment) (TableLens tagOfPost) =
  dbLenses

type TableSelector table
   = (DatabaseSettings Postgres DemoblogDb -> DatabaseEntity Postgres DemoblogDb (TableEntity table))
