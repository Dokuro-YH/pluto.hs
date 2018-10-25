{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Feature.Group.Types where

import           Foundation.Database.Imports
import           Foundation.Types.Imports
import           Foundation.Web.JSON

type GroupId = Int

data Group = Group
  { groupId        :: GroupId
  , groupName      :: Text
  , groupParentId  :: Maybe Int
  , groupDesc      :: Maybe Text
  , groupCreatedAt :: UTCTime
  , groupUpdatedAt :: UTCTime
  } deriving (Show, Eq)

data NewGroup = NewGroup
  { newGroupName     :: Text
  , newGroupParentId :: Maybe Int
  , newGroupDesc     :: Maybe Text
  } deriving (Show, Eq)

data UpdateGroup = UpdateGroup
  { updateGroupName     :: Text
  , updateGroupParentId :: Maybe Int
  , updateGroupDesc     :: Maybe Text
  } deriving (Show, Eq)

instance FromRow Group where
  fromRow = Group <$> field <*> field <*> field <*> field <*> field <*> field

$(deriveManyJSON [''Group, ''NewGroup, ''UpdateGroup])
