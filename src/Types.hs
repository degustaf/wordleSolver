{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO

data App = App
  { appLogFunc :: !LogFunc
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
