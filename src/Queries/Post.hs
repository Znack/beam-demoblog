module Queries.Post where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query

import Queries.Generic.Create
import Queries.Generic.GetAll
import Queries.Generic.GetById
import RunDB
import Schema.Database

getAll :: Pg [PostT Identity]
getAll = queryGetAll _post

getById :: Int -> Pg (Maybe (PostT Identity))
getById = queryGetByPK _post PostId

createPost :: Text -> Text -> Int -> [Int] -> Pg (Maybe (PostT Identity, [Tag]))
createPost title content authorId tags =
  runMaybeT $ do
    tagEntities <-
      MaybeT $
      fmap sequence <$> forM tags $ \tagId ->
        runSelectReturningOne .
        select .
        filter_ (\tag -> pk tag ==. (val_ . TagId . fromIntegral) tagId) . all_ $
        _tag db
    createdPost <-
      MaybeT $
      listToMaybe <$>
      BeamExtensions.runInsertReturningList
        (_post db)
        (insertExpressions
           [ Post
               default_
               (val_ title)
               (val_ content)
               (AuthorId $ fromIntegral authorId)
           ])
    let postId :: Int
        postId = unSerial $ _postId createdPost
        createTagOfPost tagId =
          TagOfPost
            default_
            (PostId $ fromIntegral postId)
            (TagId $ fromIntegral tagId)
    _ <-
      lift $
      BeamExtensions.runInsertReturningList
        (_tagOfPost db)
        (insertExpressions $ map createTagOfPost tags)
    pure (createdPost, tagEntities)
