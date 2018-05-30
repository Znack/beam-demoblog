{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Users where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data UserT f = User
  { _userId :: Columnar f Int
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar (Nullable f) Text
  , _userAvatar :: Columnar (Nullable f) Text
  , _userCreatedAt :: Columnar f UTCTime
  , _userIsAdmin :: Columnar f Bool
  } deriving (Generic, Beamable)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

User (LensFor userId) (LensFor userFirstName) (LensFor userLastName) (LensFor userAvatar) (LensFor userCreatedAt) (LensFor userIsAdmin) =
  tableLenses
