module Queries.User where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import Queries.Generic.Create
import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: Pg [User]
getAll = queryGetAll _user

getById :: Int -> Pg (Maybe User)
getById = queryGetByPK _user UserId

createUser :: User -> Pg (Maybe User)
createUser = create _user
