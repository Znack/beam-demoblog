module Queries.Generic.GetAll where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import RunDB
import Schema.Database

selectQueryAll ::
     (Table table, IsSql92SelectSyntax select, Database be db)
  => (DatabaseSettings Postgres DemoblogDb -> DatabaseEntity be db (TableEntity table))
  -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
selectQueryAll tableSelector = all_ (tableSelector db)

queryGetAll tableSelector =
  runSelectReturningList . select . selectQueryAll $ tableSelector
