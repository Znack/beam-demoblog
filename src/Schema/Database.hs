module Schema.Database where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001 (migration)
import qualified Schema.Migrations.V0002Categories as V0002 (migration)
import qualified Schema.Migrations.V0003Tags as V0003 (migration)

import Schema.Migrations.V0003Tags hiding (migration)

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
  migrationStep "Add category table" V0002.migration >>>
  migrationStep "Add tags table" V0003.migration

db :: DatabaseSettings Postgres DemoblogDb
db = unCheckDatabase (evaluateDatabase migration)
