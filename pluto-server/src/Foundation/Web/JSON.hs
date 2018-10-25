module Foundation.Web.JSON
  ( deriveJSON
  , deriveManyJSON
  ) where

import           Data.Aeson
import qualified Data.Aeson.TH       as TH
import           Language.Haskell.TH

deriveManyJSON :: [Name] -> Q [Dec]
deriveManyJSON names = concat <$> mapM deriveJSON names

deriveJSON :: Name -> Q [Dec]
deriveJSON name =
  TH.deriveJSON
    TH.defaultOptions {TH.fieldLabelModifier = fieldLabelModify}
    name
  where
    fieldLabelModify =
      camelTo2 '_' .  drop (length $ nameBase name)
