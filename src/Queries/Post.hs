module Queries.Post where

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

getAll :: Pg [PostT Identity]
getAll = queryGetAll _post

getById :: Int -> Pg (Maybe (PostT Identity))
getById = queryGetByPK _post PostId

createPost :: Post -> Pg (Maybe (PostT Identity))
createPost = create _post
