module Queries.Generic.Create where

import Data.Maybe (listToMaybe)

import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema.Tables

import GHC.Generics

import RunDB
import Schema.Database

runQuery ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg [table Identity]
runQuery table entity =
  BeamExtensions.runInsertReturningList (table db) $
  insertExpressions [val_ entity]

create ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg (Maybe (table Identity))
create table entity = listToMaybe <$> runQuery table entity
