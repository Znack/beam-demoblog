module Queries.Generic.GetById where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import Data.Maybe (listToMaybe)

import RunDB
import Schema.Database

selectQueryFilteredByPk tableSelector idvalue =
  filter_ (\entity -> pk entity ==. idvalue) $ all_ (tableSelector db)

queryGetByPK tableSelector pkConstructor =
  runSelectReturningOne .
  select .
  selectQueryFilteredByPk tableSelector . val_ . pkConstructor . fromIntegral
