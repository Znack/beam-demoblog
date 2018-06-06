module Queries.User where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import Queries.Generic.Create
import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: Pg [UserT Identity]
getAll = queryGetAll _user

getById :: Int -> Pg (Maybe (UserT Identity))
getById = queryGetByPK _user UserId

createUser :: User -> Pg (Maybe (UserT Identity))
createUser = create _user
