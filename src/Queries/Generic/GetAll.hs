module Queries.Generic.GetAll where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import RunDB
import Schema.Database

selectQueryAll ::
     (Table table, IsSql92SelectSyntax select)
  => (DatabaseSettings Postgres DemoblogDb -> DatabaseEntity Postgres DemoblogDb (TableEntity table))
  -> Q select DemoblogDb s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
selectQueryAll tableSelector = all_ (tableSelector db)

queryGetAll tableSelector =
  runSelectReturningList . select . selectQueryAll $ tableSelector
