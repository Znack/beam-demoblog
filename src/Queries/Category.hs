module Queries.Category where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query

import Queries.Generic.Create
import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: Pg [CategoryT Identity]
getAll = queryGetAll _category

getById :: Int -> Pg (Maybe (CategoryT Identity))
getById = queryGetByPK _category CategoryId

createCategory :: Category -> Pg (Maybe (CategoryT Identity))
createCategory = create _category
