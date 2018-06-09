module Queries.Category where

import Data.Text (Text)

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

createCategory :: Text -> Maybe Int -> Pg (Maybe (CategoryT Identity))
createCategory title parentId =
  createFromExpr
    _category
    (Category
       default_
       (val_ title)
       (CategoryId (val_ $ fromIntegral <$> parentId)))
