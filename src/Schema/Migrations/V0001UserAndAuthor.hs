{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Migrations.V0001UserAndAuthor where

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

User (LensFor userId) (LensFor userFirstName) (LensFor userLastName) (LensFor userAvatar) (LensFor userCreatedAt) =
  tableLenses

--
--
-- AUTHOR Model
data AuthorT f = Author
  { _authorId :: Columnar f (SqlSerial Int)
  , _authorDescription :: Columnar f Text
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

Author (LensFor authorId) (LensFor authorDescription) = tableLenses

--
-- === DATABASE DEFINITON ===
--
data DemoblogDb f = DemoblogDb
  { _user :: f (TableEntity UserT)
  , _author :: f (TableEntity AuthorT)
  } deriving (Generic)

instance Database Postgres DemoblogDb

migration ::
     ()
  -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres DemoblogDb)
migration () =
  DemoblogDb <$>
  createTable
    "user"
    (User
       (field "user_id" serial)
       (field "first_name" (varchar (Just 45)) notNull)
       (field "last_name" (maybeType $ varchar (Just 45)))
       (field "avatar" (maybeType $ varchar (Just 511)))
       (field "created_at" timestamptz (defaultTo_ now_) notNull)) <*>
  createTable
    "author"
    (Author
       (field "author_id" serial)
       (field "description" (varchar (Just 50)) notNull))
