module RunDB where

import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

createPgConn =
  Pg.connectPostgreSQL "postgresql://demoblog@localhost:5432/beam_demoblog"

runDB :: Pg a -> Connection -> IO a
runDB query conn = runBeamPostgresDebug putStrLn conn query

runDBWithSeparateConn query = createPgConn >>= runDB query
