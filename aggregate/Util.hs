module Util where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Parse
import Data.Set (Set)
import Data.Map (Map)

isStr (KStr _) = True
isStr _ = False

jaccard :: Set Const -> Set Const -> Double
jaccard ref test =
  let inter = Set.size $ Set.intersection ref test
      unned = Set.size $ Set.union ref test
  in (fromIntegral inter) / (fromIntegral unned)

toClusterMap :: [[String]] -> Map.Map String Int
toClusterMap strs = Map.fromList $ concat $ zipWith (\n -> map (\s -> (s, n))) [0..] strs

updateConstDB :: Map.Map Int (Set Const) -> (Int, [Const]) -> Map.Map Int (Set Const)
updateConstDB m (k, v) =
  let base = case Map.lookup k m of
               Just v0 -> v0
               Nothing -> Set.empty
  in Map.insert k (Set.union base (Set.fromList v)) m

toCluster cm (i, _, ks) = do
  k <- Map.lookup (tail i) cm
  return $ (k, ks)

mergeDB :: Double -> Map Int (Set Const) -> Map Int (Set Const) -> Map Int (Set Const)
mergeDB thresh db0 db1 =
  Map.foldl (\db ks ->
    case sortBy (\(_, s) (_, s') -> compare s' s) $ Map.toList $ Map.filter (\(_,s) -> s >= thresh) $ Map.mapWithKey (\i ks' -> (i, jaccard ks ks')) db of
      (i, _):_ -> Map.insert i (Set.union (db Map.! i) ks) db
      _ -> case Map.maxViewWithKey db of
            Just ((i,_),_) -> Map.insert (i + 1) ks db
            _ -> Map.insert 0 ks db) db0 db1
