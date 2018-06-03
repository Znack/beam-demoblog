{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Schema.Mutate where

import Database.Beam
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple as Pg

import Schema.Database

createPgConn =
  Pg.connectPostgreSQL "postgresql://demoblog@localhost:5432/beam_demoblog"

create :: Pg.Connection -> IO ()
create conn =
  runBeamPostgres
    conn
    (createSchema migrationBackend (evaluateDatabase migration))

migrate :: Pg.Connection -> IO ()
migrate conn =
  runBeamPostgres
    conn
    (autoMigrate migrationBackend (evaluateDatabase migration))

printMigration :: Pg.Connection -> IO [()]
printMigration conn = do
  Just cmds :: Maybe [MigrationCommand PgCommandSyntax] <-
    simpleMigration' migrationBackend conn (evaluateDatabase migration)
  mapM
    (\(MigrationCommand (PgCommandSyntax a b) isSafe) ->
       putStrLn $
       "> " ++ show isSafe ++ "" ++ show a ++ "\n" ++ show b ++ "\n\n\n")
    cmds

-- showMigrations :: Pg.Connection -> IO [()]
-- showMigrations conn = do
--   Just cmds <- createMigration conn
--   return $ map (runMigrationSilenced . (`upDown` Nothing)) cmds
-- importDbSchema conn = do
--   let cmd = MigrateCmdLine
--   return undefined
simpleMigration' ::
     (MonadBeam cmd be handle m, Database be db)
  => BeamMigrationBackend cmd be handle m
  -> handle
  -> CheckedDatabaseSettings be db
  -> IO (Maybe [MigrationCommand cmd])
simpleMigration' BeamMigrationBackend { backendGetDbConstraints = getCs
                                      , backendActionProvider = action
                                      } hdl db = do
  pre <- withDatabase hdl getCs
  let post = collectChecks db
      solver = heuristicSolver action pre post
  case finalSolution solver of
    Solved cmds -> pure (Just cmds)
    Candidates {} -> pure Nothing
