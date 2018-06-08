module Queries.Generic.GetAll where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import GHC.Generics

import RunDB
import Schema.Database

selectQueryAll ::
     (Table table)
  => TableSelector table
  -> Q PgSelectSyntax DemoblogDb s (table (QExpr PgExpressionSyntax s))
selectQueryAll tableSelector = all_ (tableSelector db)

selectAll ::
     (Table table)
  => TableSelector table
  -> SqlSelect PgSelectSyntax (table Identity)
selectAll = select . selectQueryAll

queryGetAll ::
     ( Table table
     , Generic (table Identity)
     , Generic (table Exposed)
     , FromBackendRow Postgres (table Identity)
     )
  => TableSelector table
  -> Pg [table Identity]
queryGetAll = runSelectReturningList . selectAll
