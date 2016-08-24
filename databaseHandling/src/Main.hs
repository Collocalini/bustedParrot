{-# LANGUAGE OverloadedStrings #-}

--import Data.Aeson
import Nodes
import System.Directory

--import qualified Data.Map.Strict as DMap
import Data.List
--import qualified Data.Maybe as M
import System.Environment
--import qualified Data.ByteString.Lazy as B
--import qualified Data.Text as T




main = do
  a <- getArgs
  --print a
  case a of
    [] -> run []
    _  -> do
             nu<- node_from_name $ unwords a
             run [("new",nu)]

  

run nu = do
  n <- nodes_io
  --mapM (mapM (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")) $ concat [n,nu]
  --mapM_ (putStrLn.show) $ nodesToNumberedList_fileRef $ concat [n,nu]
  
  putStrLn "Node database sanity check"
  putStrLn "Find same tags:"
  mapM (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")
    $ findSameTags_fileRef $ nodesToNumberedList_fileRef $ concat [n,nu]
  putStrLn "Find same links:"
  mapM (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")
    $ findSameLinks_fileRef $ nodesToNumberedList_fileRef $ concat [n,nu]
  


nodes_io :: IO [(String, Nodes)]
nodes_io = do
   l<- getDirectoryContents "nodes"
   --print l
   let f = filter isNodeFile l
   --print f
   d <- mapM node_from_name_suffix $ number_from_node_name f
   return $ zip f d


nodesToNumberedList_LinksOnly n = map (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
         link = l }) = (i,l)



nodesToNumberedList_fileRef n =
  concatMap (\(n,f)-> f n)
     $ zip (concatMap (\(f,ns)-> zip (repeat f) ns) n)
     $ map node_to_list [1..]
  where
    node_to_list i
      (f,
       (Node {
          node = n
         ,link = l })
      ) = zip4 (repeat i) [f] n (repeat l)



nodesToNumberedList n = concatMap (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
          node = n
         ,link = l }) = zip3 (repeat i) n $ repeat l



findSameTags l = filter notSingle
  $ groupBy (\(_,n,_) (_,n',_) -> n==n')
  $ sortOn (\(_,n,_)-> n) l
  
findSameTags_fileRef l = filter notSingle
  $ groupBy (\(_,_,n,_) (_,_,n',_) -> n==n')
  $ sortOn (\(_,_,n,_)-> n) l

notSingle []  = False
notSingle [_] = False
notSingle  _  = True





findSameLinks l = filter notSingle
  $ groupBy (\(_,_,u) (_,_,u') -> u==u')
  $ sortOn (\(_,_,u)-> u) l
  
findSameLinks_fileRef l = filter notSingle
  $ groupBy (\(_,_,_,u) (_,_,_,u') -> u==u')
  $ sortOn (\(_,_,_,u)-> u) l


