module Main where

import Parse
import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.List
import Util
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import Numeric
import Text.Printf
import Debug.Trace

thresh = 0.1

jaccardOptimal :: Map Int (Set Const) -> Map Int (Set Int, Double) -> Int -> Set Const -> Map Int (Set Int, Double)
jaccardOptimal base m n ks =
  let norm (ks, 0.0) = (ks, 0.0)
      norm (ks, qs) = (ks, qs / (fromIntegral $ traceShow (length ks) $ length ks))
      merge :: [(Int, Double)] -> (Set Int, Double)
      merge = foldl (\(s, q) (j, q') -> (Set.insert j s, max q q')) (Set.empty, 0.0)
      jaccardValid = norm $ merge $ filter (\(_, q) -> q > thresh) $ Map.toList $ Map.map (`jaccard` ks) base in
  Map.insert n jaccardValid m

abberant :: Map Int (Set Const) -> Map Int (Set Int, Double) -> Map String Int -> (String, Word64,[Const]) -> Maybe (Word64, Set Const, Double)
abberant normal sims clustering (func, addr, ks) = do
  let test = Set.fromList ks
  localCluster <- Map.lookup (tail func) clustering
  (canonClusters, quality) <- Map.lookup localCluster sims
  refs <- mapM (`Map.lookup` normal) $ Set.toList canonClusters
  let ref = Set.unions refs
  let abberations = test Set.\\ ref
  if Set.null abberations
    then Nothing
    else Just (addr, abberations, quality)

showConst (KNum k s) = printf "%08x:%d" k s
showConst (KStr s) = show s

showConsts consts =
  intercalate ", " $ map showConst $ Set.toList consts

showAddrs addrs =
  intercalate ", " $ map (`showHex` "") $ Set.toList addrs

printAbb (const, (addrs, quality)) =
  putStrLn $ showConst const ++ " " ++ showAddrs addrs ++ " " ++ show quality

updateAbberant addr quality m const =
  case Map.lookup const m of
    Just (addrs, q) -> Map.insert const (Set.insert addr addrs, max q quality) m
    Nothing -> Map.insert const (Set.singleton addr, quality) m

updateAbberants m (addr, consts, quality) =
  Set.foldl (updateAbberant addr quality) m consts

main = do
  [clusterFile, constFile, baseFile] <- getArgs
  clusterStrs <- readFile clusterFile
  constsStrs <- readFile constFile
  baseStr <- readFile baseFile

  let (clusters, consts) = parse clusterStrs constsStrs
  let clusterMap = toClusterMap clusters
  let base :: Map.Map Int (Set Const)
      base = read baseStr
  let constDB = foldl updateConstDB Map.empty $ mapMaybe (toCluster clusterMap) consts
  let clusterSim = Map.foldlWithKey (jaccardOptimal base) Map.empty constDB
  let abberations = sortBy (\(_,(_,x)) (_,(_,x')) -> compare x' x) $ Map.toList $ foldl' updateAbberants Map.empty $ mapMaybe (abberant base clusterSim clusterMap) consts
  mapM printAbb $ filter (\(k,_) -> isStr k) abberations
