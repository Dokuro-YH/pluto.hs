{-# LANGUAGE TemplateHaskell #-}

module Feature.User.Types where

import           Foundation.Types.Imports
import           Foundation.Web.Imports

import           Feature.Group.Types
import           Feature.Role.Types

type UserId = Int

type Email = Text

data PhoneNumber = PhoneNumber
  { phoneNumberPrefix :: Text
  , phoneNumber       :: Text
  } deriving (Show, Eq)

data Address = Address
  { addressProvince :: Text
  , addressCity     :: Text
  , addressCounty   :: Text
  , addressDetails  :: Maybe Text
  } deriving (Show, Eq, Read)

data UserAuthority
  = AdminAuthority
  | UserAuthority { roleId :: RoleId }
  deriving (Show, Eq)

data DataAuthority
  = AllDataAuthority
  | GroupDataAuthority
  deriving (Show, Eq)

data User = User
  { userId                :: UserId
  , userUsername          :: Text
  , userPassword          :: Text
  , userGroupId           :: GroupId
  , userAuthority         :: UserAuthority
  , userDataAuthority     :: DataAuthority
  , userCreatedAt         :: UTCTime
  , userUpdatedAt         :: UTCTime
  , userLastLogonTime     :: UTCTime
  , userPrevisonLogonTime :: UTCTime
  } deriving (Show, Eq)

data UserInfo = UserInfo
  { userInfoUserId  :: UserId
  , userInfoName    :: Text
  , userInfoTitle   :: Text
  , userInfoGroup   :: Text
  , userInfoPhone   :: Maybe PhoneNumber
  , userInfoEmail   :: Maybe Email
  , userInfoAddress :: Maybe Address
  } deriving (Show, Eq)

data UpdateUser = UpdateUser
  { updateUserGroupId       :: GroupId
  , updateUserAuthority     :: UserAuthority
  , updateUserDataAuthority :: DataAuthority
  } deriving (Show, Eq)

data UpdateUserInfo = UpdateUserInfo
  { updateUserInfoName    :: Text
  , updateUserInfoPhone   :: Maybe PhoneNumber
  , updateUserInfoEmail   :: Maybe Email
  , updateUserInfoAddress :: Maybe Address
  } deriving (Show, Eq)

data UserChangePassword = UserChangePassword
  { userChangePasswordPassword        :: Text
  , userChangePasswordNewPassword     :: Text
  , userChangePasswordConfirmPassword :: Text
  } deriving (Show, Eq)

$(deriveManyJSON
    [ ''UserAuthority
    , ''DataAuthority
    , ''PhoneNumber
    , ''Address
    , ''User
    , ''UserInfo
    , ''UpdateUser
    , ''UpdateUserInfo
    , ''UserChangePassword
    ])
