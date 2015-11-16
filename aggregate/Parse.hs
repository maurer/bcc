{-# LANGUAGE ScopedTypeVariables #-}
module Parse where

import System.IO
import Control.Applicative ()
import Data.List
import Data.Word
import qualified Data.Map as Map

data Const =
    KNum Word64 Int
  | KStr String deriving (Read, Show, Eq, Ord)

explodeConstPool :: String -> [Const]
explodeConstPool ('[':':':' ':str) =
  explodeConstPool' str

explodeConstPool' :: String -> [Const]
explodeConstPool' n@('0':'x':_) =
  let [(k :: Word64, ':':r)] = reads n
      [(s :: Int, r')] = reads r in
  (KNum k s) : explodeConstPool' r'
explodeConstPool' s@('"':_) =
  let [(k :: String, r)] = reads s in
  (KStr k) : explodeConstPool' r
explodeConstPool' "; :]" = []
explodeConstPool' (';':' ':r) = explodeConstPool' r

explodeConst :: String -> (String, Word64, [Const])
explodeConst constLine =
  let (p1:p2:_)           = elemIndices ':' constLine
      (parent, _:ls)      = splitAt p1 constLine
      (addrStr, _:constStr) = splitAt (p2 - p1 - 1) ls
      (addr :: Word64) = read $ "0x" ++ addrStr 
      consts = explodeConstPool constStr
  in (parent, addr, consts)

loadClusters :: [String] -> [[String]]
loadClusters recs =
  let updateMap :: Map.Map String [String] -> [String] -> Map.Map String [String]
      updateMap m (k : vvs) =
        let v = unwords vvs in
        case Map.lookup k m of
           Just vs -> Map.insert k (v:vs) m
           Nothing -> Map.insert k [v] m
  in Map.elems $ foldl' updateMap Map.empty (map words recs)

parse clusters consts =
  (loadClusters $ lines clusters, map explodeConst $ lines consts)
