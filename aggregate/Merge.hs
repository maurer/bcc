module Main where

import Parse
import System.Environment
import System.IO
import Data.Maybe
import Util
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)

main = do
  dbFiles <- getArgs
  dbStrs <- mapM readFile dbFiles
  let dbs :: [Map Int (Set Const)]
      dbs = map read dbStrs
  print $ foldl1 (mergeDB 0.9) dbs
