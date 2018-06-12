{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Schema.Mutate where

import qualified Data.Text as T
import Database.Beam
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.Syntax
import qualified Database.PostgreSQL.Simple as Pg

import qualified Schema.Migrations.V0001UserAndAuthor as V0001
import Schema.Migrations.V0002ExampleBlog
import qualified Schema.Migrations.V0002UserTableIsAdmin as V0002
import qualified Schema.Migrations.V0003Category as V0003

createDbSchema :: Pg.Connection -> IO ()
createDbSchema conn =
  runBeamPostgres
    conn
    (createSchema migrationBackend (evaluateDatabase migration))

printMigration :: Pg.Connection -> IO [()]
printMigration conn = do
  Just cmds :: Maybe [PgCommandSyntax] <-
    simpleMigration migrationBackend conn (evaluateDatabase migration)
  mapM
    (\(PgCommandSyntax a b) ->
       putStrLn $ "> " ++ show a ++ "\n" ++ show b ++ "\n\n\n")
    cmds

verboseHooks :: BringUpToDateHooks Pg
verboseHooks =
  BringUpToDateHooks
  { runIrreversibleHook = pure True
  , startStepHook =
      \a b -> liftIO (print $ "startStepHook N" ++ show a ++ ": " ++ show b)
  , endStepHook =
      \a b -> liftIO (print $ "endStepHook N" ++ show a ++ ": " ++ show b)
  , runCommandHook =
      \a b -> liftIO (print $ "runCommandHook N" ++ show a ++ ": " ++ show b)
  , queryFailedHook = fail "Log entry query fails"
  , discontinuousMigrationsHook =
      \ix ->
        fail ("Discontinuous migration log: missing migration at " ++ show ix)
  , logMismatchHook =
      \ix actual expected ->
        fail
          ("Log mismatch at index " ++
           show ix ++
           ":\n" ++
           "  expected: " ++
           T.unpack expected ++ "\n" ++ "  actual  : " ++ T.unpack actual)
  , databaseAheadHook =
      \aheadBy ->
        fail
          ("The database is ahead of the known schema by " ++
           show aheadBy ++ " migration(s)")
  }

migrateWithHooks conn =
  runBeamPostgres
    conn
    (bringUpToDateWithHooks verboseHooks migrationBackend migration)
