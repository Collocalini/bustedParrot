-----------------------------------------------------------------------------
--
-- Module      :  Nodes
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Nodes (
 Nodes
,Node (..)
,Node_map
,nodesT_io
,node_to_link
,nodes_to_map
) where


import Data.Aeson
--import Data.List
import qualified Data.Map as Dm
import GHC.Generics
import qualified Data.Maybe as M
import qualified Data.Text as T
import System.Directory
import Control.Applicative
import qualified Data.ByteString.Lazy as B



type Node_map = Dm.Map T.Text T.Text



data Node = Node {
  node :: [T.Text]
 ,link :: T.Text
} deriving (Show,Eq)


data Node_json = Node_json {
  node_json :: [T.Text]
 ,link_json :: T.Text
} deriving (Show,Generic)



type Nodes_json = [Node_json]
type Nodes = [Node]


instance FromJSON Node_json
instance ToJSON Node_json




nodesT_io :: IO Nodes
nodesT_io = do
   l<- getDirectoryContents "nodes"
   d <- mapM node_from_name_suffix $ number_from_node_name $ filter isNodeFile l
   return $ concat d





isNodeFile :: FilePath -> Bool
isNodeFile ('n':'o':'d':'e':'s':'_':_) = True
isNodeFile _ = False



node_from_name_suffix :: String -> IO Nodes
node_from_name_suffix s = do
   d <- eitherDecode <$> B.readFile ("nodes/nodes_" ++ s ++ ".json"):: IO (Either String Nodes_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map give_node dl



node_from_name :: String -> IO Nodes
node_from_name s = do
   d <- eitherDecode <$> B.readFile s :: IO (Either String Nodes_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map give_node dl



number_from_node_name :: [FilePath] -> [String]
number_from_node_name fp = map (n . (drop 6)) fp
       where
         n x = (take ((length x)-5) ) x





give_node :: Node_json -> Maybe Node
give_node     (Node_json {node_json = n
                         ,link_json = l})

   |has_nodes = Just (Node {
      node = sane_nodes
     ,link = l
     })
   |otherwise = Nothing
   where
   has_nodes = not $ null sane_nodes
   sane_nodes = filter (\x-> not $ T.null x) n








node_to_link :: T.Text -> Node_map -> T.Text
node_to_link n nm = Dm.findWithDefault "" n nm





nodes_to_map :: Nodes -> Node_map
nodes_to_map nodes = Dm.fromList $ concat $ map node_to_list nodes
   where
   node_to_list (Node {
      node = n
     ,link = l
              }) = zip n $ repeat l







