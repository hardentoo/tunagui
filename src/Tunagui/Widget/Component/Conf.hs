module Tunagui.Widget.Component.Conf where

data DimConf a = DimConf
  {
    maxv :: Maybe a
  , minv :: Maybe a
  , padding1 :: a
  , padding2 :: a
  , margin1 :: a
  , margin2 :: a
  } deriving Show
