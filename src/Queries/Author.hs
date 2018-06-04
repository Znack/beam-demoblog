module Queries.Author where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query

import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: IO ()
getAll =
  createPgConn >>= runDB (queryGetAll author) >>=
  mapM_ (liftIO . putStrLn . show)

getById :: Integer -> IO ()
getById idval =
  createPgConn >>=
  runDB (queryGetByPK author (val_ $ AuthorId (fromInteger idval))) >>=
  mapM_ (liftIO . putStrLn . show)
