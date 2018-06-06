module Queries.Generic.Create where

import Data.Maybe (listToMaybe)

import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Postgres

import RunDB
import Schema.Database

create table entity =
  fmap listToMaybe <$> BeamExtensions.runInsertReturningList (table db) $
  insertExpressions [val_ entity]
