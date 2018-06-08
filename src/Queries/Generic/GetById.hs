module Queries.Generic.GetById where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema.Tables

import GHC.Generics

import RunDB
import Schema.Database

selectQueryFilteredByPk ::
     ( Table table
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     )
  => TableSelector table
  -> PrimaryKey table (QExpr PgExpressionSyntax s)
  -> Q PgSelectSyntax DemoblogDb s (table (QExpr PgExpressionSyntax s))
selectQueryFilteredByPk tableSelector idvalue =
  filter_ (\entity -> pk entity ==. idvalue) $ all_ (tableSelector db)

selectByPK ::
     ( Table table
     , Integral a
     , Num b
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     )
  => TableSelector table
  -> (b -> PrimaryKey table Identity)
  -> a
  -> SqlSelect PgSelectSyntax (table Identity)
selectByPK tableSelector pkConstructor =
  select .
  selectQueryFilteredByPk tableSelector . val_ . pkConstructor . fromIntegral

queryGetByPK ::
     ( Table table
     , Integral a
     , Num b
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     , FromBackendRow Postgres (table Identity)
     )
  => TableSelector table
  -> (b -> PrimaryKey table Identity)
  -> a
  -> Pg (Maybe (table Identity))
queryGetByPK tableSelector pkConstructor =
  runSelectReturningOne . selectByPK tableSelector pkConstructor
