module Schema.Database where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001 (migration)
import qualified Schema.Migrations.V0002Categories as V0002 (migration)

import Schema.Migrations.V0002Categories hiding (migration)

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
  migrationStep "Initial commit" V0001.migration >>>
  migrationStep "Add category table" V0002.migration

db :: DatabaseSettings Postgres DemoblogDb
db = unCheckDatabase (evaluateDatabase migration)
