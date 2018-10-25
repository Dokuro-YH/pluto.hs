{-# LANGUAGE FlexibleContexts #-}

module Feature.Auth.Types where

import           Foundation.Types.Imports

import           Feature.User.Types

type Permission = Text

type Permissions = Set Permission

data AuthenticationError
  = BadCredentials
  | NotFound
  deriving (Show, Eq)

class Authentication a where
  identity :: a -> Text
  credentials :: a -> Text
  authorities :: a -> Permissions
  userInfo :: a -> UserInfo

-- class AuthenticationManager am where
--   authenticate ::
--        ( Monad m
--        , MonadIO m
--        , MonadError AuthenticationError m
--        , Authentication a
--        , Authentication r
--        )
--     => am -- ^ Self
--     -> a -- ^ Authentication token.
--     -> m r -- ^ Authentication result.
