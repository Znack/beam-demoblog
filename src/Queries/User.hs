module Queries.User where

import Data.Text (Text)

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

createUser :: Text -> Maybe Text -> Maybe Text -> Bool -> Pg (Maybe User)
createUser fName lName avatar isAdmin =
  createFromExpr
    _user
    (User
       default_
       (val_ fName)
       (val_ lName)
       (val_ avatar)
       currentTimestamp_
       (val_ isAdmin))
