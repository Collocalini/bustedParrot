{-# LANGUAGE OverloadedStrings #-}

--import Data.Aeson
import Nodes

--import qualified Data.Map.Strict as DMap
import Data.List
--import qualified Data.Maybe as M
--import System.Environment
--import qualified Data.ByteString.Lazy as B
--import qualified Data.Text as T




main = do
  a <- getContents
  n <- nodesT_io
  nu<- node_from_name a
  
  putStrLn "Node database sanity check"
  putStrLn "Find same tags:"
  print $ findSameTags $ nodesToNumberedList $ concat [n,nu]
  putStrLn "Find same links:"
  print $ findSameLinks $ nodesToNumberedList_LinksOnly $ concat [n,nu]
  
  


nodesToNumberedList_LinksOnly n = map (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
         ,link = l }) = (i,l)



nodesToNumberedList n = concatMap (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
          node = n
         ,link = l }) = zip3 (repeat i) n $ repeat l



findSameTags l = filter notSingle $ groupBy (\(_,n,_) (_,n',_) -> n==n') l

notSingle []  = False
notSingle [_] = False
notSingle  _  = True



findSameLinks l = filter notSingle $ groupBy (\(_,u) (_,u') -> u==u') l


