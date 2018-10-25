module Foundation.Filter.Parser where

import           Foundation.Filter.Types

import           Text.Parsec

parseEither :: String -> Either ParseError Filter
parseEither = undefined

parseMaybe :: String -> Maybe Filter
parseMaybe = undefined
