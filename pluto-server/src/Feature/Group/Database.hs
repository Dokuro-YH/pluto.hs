module Feature.Group.Database where

import           Foundation.Database.Imports

import           Data.Maybe                  (listToMaybe)
import           Data.Time                   (getCurrentTime)

import           Feature.Group.Types

findAll :: MonadDb m => m [Group]
findAll = runQuery_ "select id,name,parent_id,description,created_at,updated_at from groups"

findById :: MonadDb m => GroupId -> m (Maybe Group)
findById id' = listToMaybe <$> runQuery sql' [id']
  where
    sql' = "select id,name,parent_id,description,created_at,updated_at from groups where id = ?"

create :: MonadDb m => NewGroup -> m Group
create NewGroup {..} =
  runReturning' sql' (newGroupName, newGroupParentId, newGroupDesc)
  where
    sql' =
      mconcat
        [ " insert into groups (name,parent_id,description)"
        , " values (?,?,?)"
        , " returning id,name,parent_id,description,created_at,updated_at"
        ]

update :: MonadDb m => GroupId -> UpdateGroup -> m (Maybe Group)
update id' UpdateGroup {..} = do
  now <- liftIO getCurrentTime
  runReturning
    sql'
    (updateGroupName, updateGroupParentId, updateGroupDesc, now, id')
  where
    sql' =
      mconcat
        [ " update groups"
        , " set name=?, parent_id=?, description=?, updated_at=?"
        , " where id=?"
        , " returning id,name,parent_id,description,created_at,updated_at"
        ]

deleteById :: MonadDb m => GroupId -> m (Maybe Group)
deleteById id' = do
  group <- findById id'
  case group of
    Nothing -> return Nothing
    Just g  -> Just <$> delete g
  where
    delete g@Group{..} = runUpdate sql' [id'] >> return g
    sql' = "delete from groups where id=?"
