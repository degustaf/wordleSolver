{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
-- import RIO.Process
-- import Options.Applicative.Simple
-- import qualified Paths_wordleSolver

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          }
     in runRIO app run
