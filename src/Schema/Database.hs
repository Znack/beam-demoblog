module Schema.Database where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001 (migration)
import qualified Schema.Migrations.V0002UserTableIsAdmin as V0002 (migration)
import qualified Schema.Migrations.V0003Categories as V0003 (migration)
import qualified Schema.Migrations.V0004Tags as V0004 (migration)

import Schema.Migrations.V0004Tags hiding (migration)

import Control.Arrow

import Database.Beam (DatabaseSettings)
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
  migrationStep "Add tags table" V0004.migration

db :: DatabaseSettings Postgres DemoblogDb
db = unCheckDatabase (evaluateDatabase migration)
