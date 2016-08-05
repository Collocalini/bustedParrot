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
,node_is_an_svg
,nodes_to_map
,link_is_local
,page_node_link
,post_node_link
,page_node_link'
,post_node_link'
,archive_pageN_node_link''
,archive_latest_first_pageN_node_link''
,dippers_pageN_node_link
,dippers_pageN_node_link'
,dippers_pageN_node_link''
,dipper_page_node_link'
,individual_dipper_node_link
,individual_dipper_node_link'
,individual_dipper_node_link''
,individual_dipper_tagged_node_link
,individual_dipper_tagged_node_link'
,individual_dipper_tagged_node_link''
,individual_dipper_tagged_page_link
,individual_dipper_tagged_page_link'
,individual_dipper_tagged_request_link
,tagged_node_link
,tagged_node_link'
,tagged_node_link''
,tagged_tag_link
,tagged_tag_link''
,tagged_tag
,tagged_dipper
,tagged_dipper''
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
import qualified Data.ByteString.Char8 as B8
import qualified System.FilePath.Posix as Fp

import Dipper_common

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

node_is_an_svg :: T.Text -> Bool
node_is_an_svg m = (T.reverse $ T.take 5 $ T.reverse m) == ".svg}"



page_node_link :: String -> B8.ByteString
page_node_link n = B8.pack $ "/pages/page" ++ n ++ ".html"

page_node_link' :: String -> T.Text
page_node_link' n = T.pack $ "/pages/page" ++ n ++ ".html"


post_node_link :: Int -> B8.ByteString
post_node_link n = B8.pack $ "/post" ++ (show n) ++ ".html"

post_node_link' :: Int -> T.Text
post_node_link' n = T.pack $ "/post" ++ (show n) ++ ".html"



archive_pageN_node_link'' :: Int -> String
archive_pageN_node_link'' n = "/archive" ++ (show n) ++ ".html"

archive_latest_first_pageN_node_link'' :: Int -> String
archive_latest_first_pageN_node_link'' n = "/archive_latest_first" ++ (show n) ++ ".html"



dippers_pageN_node_link :: Int -> B8.ByteString
dippers_pageN_node_link n = B8.pack $ "/dippers/dippers_" ++ (show n) ++ ".html"

dippers_pageN_node_link' :: Int -> T.Text
dippers_pageN_node_link' n = T.pack $ "/dippers/dippers_" ++ (show n) ++ ".html"

dippers_pageN_node_link'' :: Int -> String
dippers_pageN_node_link'' n = "/dippers/dippers_" ++ (show n) ++ ".html"


dipper_page_node_link' :: T.Text -> T.Text
dipper_page_node_link' u
   |link_is_local u = replace_extention_only
   |otherwise       = strip_path_and_replace_extention
   where
   replace_extention_only = T.pack $ Fp.replaceExtension (T.unpack u) ".html"
   strip_path_and_replace_extention =
      T.pack $ "/" ++ Fp.replaceExtension (Fp.takeFileName $ T.unpack u) ".html"





individual_dipper_node_link :: String -> B8.ByteString
individual_dipper_node_link ('/':n) = B8.pack $ "/individual_dippers/" ++ n
individual_dipper_node_link n = B8.pack $ "/individual_dippers/" ++ n

individual_dipper_node_link' :: String -> T.Text
individual_dipper_node_link' ('/':n) = T.pack $ "/individual_dippers/" ++ n
individual_dipper_node_link' n = T.pack $ "/individual_dippers/" ++ n

individual_dipper_node_link'' :: String -> String
individual_dipper_node_link'' ('/':n) = "/individual_dippers/" ++ n
individual_dipper_node_link'' n = "/individual_dippers/" ++ n


individual_dipper_tagged_page_link :: Dipper -> String -> B8.ByteString
individual_dipper_tagged_page_link d tags = B8.pack $ "/tagged/" ++ tags ++ (T.unpack $ cmp d)
     where
     cmp d = (\(Dipper {page_url  = p}) -> p) d


individual_dipper_tagged_page_link' :: Dipper -> String -> T.Text
individual_dipper_tagged_page_link' d tags = T.pack $ "/tagged/" ++ tags ++ (T.unpack $ cmp d)
     where
     cmp d = (\(Dipper {page_url  = p}) -> p) d


individual_dipper_tagged_node_link :: Dipper -> B8.ByteString
individual_dipper_tagged_node_link d = B8.pack $ tagged_tag_link'' ++ (T.unpack $ cmp d)
     where
     cmp d = (\(Dipper {page_url  = p}) -> p) d


individual_dipper_tagged_node_link' :: Dipper -> T.Text
individual_dipper_tagged_node_link' d = T.pack $ tagged_tag_link'' ++ (T.unpack $ cmp d)
     where
     cmp d = (\(Dipper {page_url  = p}) -> p) d


individual_dipper_tagged_node_link'' :: Dipper -> String
individual_dipper_tagged_node_link'' d = tagged_tag_link'' ++ (T.unpack $ cmp d)
     where
     cmp d = (\(Dipper {page_url  = p}) -> p) d



individual_dipper_tagged_request_link :: B8.ByteString
individual_dipper_tagged_request_link  =
   B8.pack $ tagged_tag_link'' ++ "/:" ++ tagged_dipper'' ++ "/:" ++ tagged_dipper'' ++ "/:" ++ tagged_dipper''



tagged_node_link :: String -> Int -> B8.ByteString
tagged_node_link n p = B8.pack $ "/tagged/" ++ n ++ (show p) ++ ".html"

tagged_node_link' :: String -> Int -> T.Text
tagged_node_link' n p = T.pack $ "/tagged/" ++ n ++ (show p) ++ ".html"

tagged_node_link'' :: String -> Int -> String
tagged_node_link'' n p = "/tagged/" ++ n ++ (show p) ++ ".html"



tagged_tag_link :: B8.ByteString
tagged_tag_link = B8.pack $ "/tagged/:tag"

tagged_tag_link'' :: String
tagged_tag_link'' = "/tagged/:tag"

tagged_tag :: B8.ByteString
tagged_tag = B8.pack $ "tag"


tagged_dipper :: B8.ByteString
tagged_dipper = B8.pack $ "dipper"

tagged_dipper'' :: String
tagged_dipper'' = "dipper"

link_is_local :: T.Text -> Bool
link_is_local l
  |T.isPrefixOf "/static/" l = True
  |T.isPrefixOf "127.0.0.1:8080/ipfs/" l = True
  |T.isPrefixOf "http://127.0.0.1:8080/ipfs/" l = True
  |T.isPrefixOf "https://127.0.0.1:8080/ipfs/" l = True
  |T.isPrefixOf "127.0.0.1:8080/ipns/" l = True
  |T.isPrefixOf "http://127.0.0.1:8080/ipns/" l = True
  |T.isPrefixOf "https://127.0.0.1:8080/ipns/" l = True
  |T.isPrefixOf "ipfs.io/ipfs/" l = True
  |T.isPrefixOf "http://ipfs.io/ipfs/" l = True
  |T.isPrefixOf "https://ipfs.io/ipfs/" l = True
  |T.isPrefixOf "ipfs.io/ipns/" l = True
  |T.isPrefixOf "http://ipfs.io/ipns/" l = True
  |T.isPrefixOf "https://ipfs.io/ipns/" l = True
  |T.isPrefixOf "gateway.ipfs.io/ipfs/" l = True
  |T.isPrefixOf "http://gateway.ipfs.io/ipfs/" l = True
  |T.isPrefixOf "https://gateway.ipfs.io/ipfs/" l = True
  |T.isPrefixOf "gateway.ipfs.io/ipns/" l = True
  |T.isPrefixOf "http://gateway.ipfs.io/ipns/" l = True
  |T.isPrefixOf "https://gateway.ipfs.io/ipns/" l = True
  |otherwise = False

nodes_to_map :: Nodes -> Node_map
nodes_to_map nodes = Dm.fromList $ concat $ map node_to_list nodes
   where
   node_to_list (Node {
      node = n
     ,link = l
              }) = zip n $ repeat l








