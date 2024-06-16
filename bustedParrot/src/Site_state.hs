-----------------------------------------------------------------------------
--
-- Module      :  Site_state
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

module Site_state (
 Routes(..)
,max_items_per_page
,max_items_per_page_archive
) where
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.Char8 as B8

import qualified Main_page_common as MP
import qualified Page_common as Pa
import qualified Dipper_common as Dc
import qualified Nodes as N

data Routes = Routes {
 postsT :: [MP.PostT]
,pagesT :: [Pa.PageT]
,dippersT :: Dc.Dippers
,dippers_references :: [(Dc.Dipper,[MP.PostT])]
,dippers_tags :: [(Dc.Dipper,[Either String ByteString])]
,node_map :: N.Node_map
,number_of_pages_in_archive :: Int
}

max_items_per_page:: Int
max_items_per_page = 50


max_items_per_page_archive:: Int
max_items_per_page_archive = 50
