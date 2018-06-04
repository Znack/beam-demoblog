module Queries.User where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: IO [UserT Identity]
getAll = createPgConn >>= runDB (queryGetAll user)

getById :: Integer -> IO ()
getById idval =
  createPgConn >>= runDB (queryGetByPK user (val_ $ UserId (fromInteger idval))) >>=
  mapM_ (liftIO . putStrLn . show)
