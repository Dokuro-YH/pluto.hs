module Feature.Role.Types where

import           Foundation.Types.Imports

import           Feature.Group.Types

type RoleId = Int

data Role = Role { roleId          :: RoleId
                 , roleName        :: Text
                 , roleGroupId     :: GroupId
                 , rolePermissions :: Set Text
                 , roleDesc        :: Maybe Text
                 , roleCreatedAt   :: UTCTime
                 , roleUpdatedAt   :: UTCTime
                 } deriving (Show, Eq)

data NewRole = NewRole
  { newRoleName        :: Text
  , newRoleGroupId     :: GroupId
  , newRolePermissions :: Set Text
  , newRoleDesc        :: Maybe Text
  } deriving (Show, Eq)

data UpdateRole = UpdateRole
  { updateRoleName        :: Text
  , updateRolePermissions :: Set Text
  , updateRoleDesc        :: Maybe Text
  } deriving (Show, Eq)
