{-# LANGUAGE GADTs #-}

module Foundation.Filter.Types where

data FAttr =
  FAttr String

data FValue
  = FString String
  | FInteger Integer
  | FDouble Double
  | FBoolean Bool

data Filter where
  Present :: FAttr -> Filter
  Equal :: FAttr -> FValue -> Filter
  NotEqual :: FAttr -> FValue -> Filter
  Contains :: FAttr -> FValue -> Filter
  StartsWith :: FAttr -> FValue -> Filter
  EndsWith :: FAttr -> FValue -> Filter
  GreaterThan :: FAttr -> FValue -> Filter
  GreaterThenOrEqual :: FAttr -> FValue -> Filter
  LessThan :: FAttr -> FValue -> Filter
  LessThenOrEqual :: FAttr -> FValue -> Filter
  And :: Filter -> Filter -> Filter
  Or :: Filter -> Filter -> Filter
  Not :: Filter -> Filter
