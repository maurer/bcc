module Main where

import Parse
import System.Environment
import System.IO
import Data.Maybe
import Util
import qualified Data.Map as Map

main = do
  [clusterFile, constFile] <- getArgs
  clusterStrs <- readFile clusterFile
  constsStrs <- readFile constFile
  let (clusters, consts) = parse clusterStrs constsStrs
  let clusterMap = toClusterMap clusters
  let constDB = foldl updateConstDB Map.empty $ mapMaybe (toCluster clusterMap) consts
  print $ constDB
