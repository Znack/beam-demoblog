module Queries.Generic.GetById where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import RunDB
import Schema.Database

selectQueryFilteredByPk tableSelector idvalue =
  filter_ (\entity -> pk entity ==. idvalue) $ all_ (tableSelector db)

queryGetByPK tableSelector idvalue =
  runSelectReturningList . select . selectQueryFilteredByPk tableSelector $
  idvalue
