module Queries.Author where

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

getAll :: Pg [AuthorT Identity]
getAll = queryGetAll _author

getById :: Int -> Pg (Maybe (AuthorT Identity))
getById = queryGetByPK _author AuthorId

createAuthor :: Text -> Int -> Pg (Maybe (AuthorT Identity))
createAuthor description userId =
  createFromExpr
    _author
    (Author default_ (val_ description) (UserId $ val_ $ fromIntegral userId))
