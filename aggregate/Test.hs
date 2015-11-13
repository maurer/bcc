module Main where

import Parse
import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.List
import Util
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import Numeric
import Text.Printf

jaccardOptimal :: Map Int (MultiSet Const) -> Map Int (Int, Double) -> Int -> MultiSet Const -> Map Int (Int, Double)
jaccardOptimal base m n ks =
  let jaccardBest = maximumBy (\(_,x) (_,x') -> compare x x') $ Map.toList $ Map.map (`jaccard` ks) base in
  Map.insert n jaccardBest m

abberant :: Map Int (MultiSet Const) -> Map Int (Int, Double) -> Map String Int -> (String, Word64,[Const]) -> Maybe (Word64, Set Const, Double)
abberant normal sims clustering (func, addr, ks) = do
  let test = Set.fromList ks
  localCluster <- Map.lookup (tail func) clustering
  (canonCluster, quality) <- Map.lookup localCluster sims
  ref <- fmap MultiSet.toSet $ Map.lookup canonCluster normal
  let abberations = test Set.\\ ref
  if Set.null abberations
    then Nothing
    else Just (addr, abberations, quality)

showConst (KNum k s) = printf "%08x:%d" k s
showConst (KStr s) = show s

showConsts consts =
  intercalate ", " $ map showConst $ Set.toList consts

printAbb (addr, consts, quality) =
  putStrLn $ (printf "%08x\t" addr) ++ showConsts consts ++ " " ++ show quality

main = do
  [clusterFile, constFile, baseFile] <- getArgs
  clusterStrs <- readFile clusterFile
  constsStrs <- readFile constFile
  baseStr <- readFile baseFile

  let (clusters, consts) = parse clusterStrs constsStrs
  let clusterMap = toClusterMap clusters
  let base :: Map.Map Int (MultiSet Const)
      base = read baseStr
  let constDB = foldl updateConstDB Map.empty $ mapMaybe (toCluster clusterMap) consts
  let clusterSim = Map.foldlWithKey (jaccardOptimal base) Map.empty constDB
  let abberations = sortBy (\(_,_,x) (_,_,x') -> compare x' x) $ mapMaybe (abberant base clusterSim clusterMap) consts
  mapM printAbb abberations
