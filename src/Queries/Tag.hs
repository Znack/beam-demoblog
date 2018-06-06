module Queries.Tag where

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

getAll :: Pg [TagT Identity]
getAll = queryGetAll _tag

getById :: Int -> Pg (Maybe (TagT Identity))
getById = queryGetByPK _tag TagId

createTag :: Tag -> Pg (Maybe (TagT Identity))
createTag = create _tag
