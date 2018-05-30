{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Schema.Authors where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)
import Schema.Users

data AuthorT f = Author
  { _authorId :: Columnar f Int
  , _authorDescription :: Columnar f Text
  , _authorUserId :: PrimaryKey UserT f
  } deriving (Generic, Beamable)

type Author = AuthorT Identity

type AuthorId = PrimaryKey AuthorT Identity

deriving instance Show Author

deriving instance Eq Author

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (Columnar f Int)
                          deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

deriving instance Show (PrimaryKey AuthorT Identity)

Author (LensFor authorId) (LensFor authorDescription) (UserId (LensFor authorUserId)) =
  tableLenses
