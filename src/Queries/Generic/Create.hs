{-# LANGUAGE RankNTypes #-}

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

runQueryFromValues ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg [table Identity]
runQueryFromValues table entity =
  BeamExtensions.runInsertReturningList (table db) $
  insertExpressions [val_ entity]

createFromValues ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg (Maybe (table Identity))
createFromValues table entity = listToMaybe <$> runQueryFromValues table entity

runQueryFromExpr ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> (forall s. table (QExpr PgExpressionSyntax s))
  -> Pg [table Identity]
runQueryFromExpr table entity =
  BeamExtensions.runInsertReturningList (table db) $ insertExpressions [entity]

createFromExpr ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> (forall s. table (QExpr PgExpressionSyntax s))
  -> Pg (Maybe (table Identity))
createFromExpr table entity = listToMaybe <$> runQueryFromExpr table entity
