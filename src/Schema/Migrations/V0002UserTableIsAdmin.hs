{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Migrations.V0002UserTableIsAdmin
  ( module Schema.Migrations.V0001UserAndAuthor
  , module Schema.Migrations.V0002UserTableIsAdmin
  ) where

import qualified Schema.Migrations.V0001UserAndAuthor as V0001 hiding
  ( PrimaryKey(UserId)
  )
import Schema.Migrations.V0001UserAndAuthor hiding
  ( Author
  , AuthorId
  , AuthorT(..)
  , DemoblogDb(..)
  , PrimaryKey(UserId)
  , User
  , UserId
  , UserT(..)
  , authorDescription
  , authorId
  , authorUserId
  , migration
  , userAvatar
  , userCreatedAt
  , userFirstName
  , userId
  , userIsAdmin
  , userLastName
  )

import Control.Lens
import Data.Text (Text)
import Data.Time (LocalTime)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Migrate
import Database.Beam.Postgres

-- === MODELS ===
--
-- USER Model
data UserT f = User
  { _userId :: Columnar f (SqlSerial Int)
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar f (Maybe Text)
  , _userAvatar :: Columnar f (Maybe Text)
  , _userCreatedAt :: Columnar f LocalTime
  , _userIsAdmin :: Columnar f Bool
  } deriving (Generic, Beamable)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

User (LensFor userId) (LensFor userFirstName) (LensFor userLastName) (LensFor userAvatar) (LensFor userCreatedAt) (LensFor userIsAdmin) =
  tableLenses

--
--
-- AUTHOR Model
data AuthorT f = Author
  { _authorId :: Columnar f (SqlSerial Int)
  , _authorDescription :: Columnar f Text
  , _authorUserId :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type Author = AuthorT Identity

type AuthorId = PrimaryKey AuthorT Identity

deriving instance Show Author

deriving instance Eq Author

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (Columnar f (SqlSerial Int))
                          deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

deriving instance Show (PrimaryKey AuthorT Identity)

deriving instance Eq (PrimaryKey AuthorT Identity)

Author (LensFor authorId) (LensFor authorDescription) (UserId (LensFor authorUserId)) =
  tableLenses

-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _author :: f (TableEntity AuthorT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

currentDb :: CheckedDatabaseSettings Postgres DemoblogDb
currentDb = defaultMigratableDbSettings @PgCommandSyntax

migration ::
     CheckedDatabaseSettings Postgres V0001.DemoblogDb
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration oldDb = DemoblogDb <$> alterUserTable <*> preserve (_author currentDb)
  where
    alterUserTable = alterTable (V0001._user oldDb) tableMigration
    tableMigration oldTable =
      User
        (V0001._userId oldTable)
        (V0001._userFirstName oldTable)
        (V0001._userLastName oldTable)
        (V0001._userAvatar oldTable)
        (V0001._userCreatedAt oldTable) <$>
      addColumn (field "is_admin" boolean (defaultTo_ (val_ True)) notNull)
