module Util where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Parse
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import Data.MultiSet (MultiSet)

jaccard :: MultiSet Const -> MultiSet Const -> Double
jaccard ref test =
  let inter = MultiSet.size $ MultiSet.intersection ref test
      unned = MultiSet.size $ MultiSet.maxUnion ref test
  in (fromIntegral inter) / (fromIntegral unned)

toClusterMap :: [[String]] -> Map.Map String Int
toClusterMap strs = Map.fromList $ concat $ zipWith (\n -> map (\s -> (s, n))) [0..] strs

updateConstDB :: Map.Map Int (MultiSet Const) -> (Int, [Const]) -> Map.Map Int (MultiSet Const)
updateConstDB m (k, v) =
  let base = case Map.lookup k m of
               Just v0 -> v0
               Nothing -> MultiSet.empty
  in Map.insert k (MultiSet.union base (MultiSet.fromList v)) m

toCluster cm (i, _, ks) = do
  k <- Map.lookup (tail i) cm
  return $ (k, ks)


