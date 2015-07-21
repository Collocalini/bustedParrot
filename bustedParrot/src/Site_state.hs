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
) where

import qualified Main_page_common as MP
import qualified Page_common as Pa
import qualified Dipper_common as Dc
import qualified Nodes as N

data Routes = Routes {
 postsT :: [MP.PostT]
,pagesT :: [Pa.PageT]
,dippersT :: Dc.Dippers
,dippers_references :: [(Dc.Dipper,[MP.PostT])]
,dippers_tags :: [(Dc.Dipper,[String])]
,node_map :: N.Node_map
}

max_items_per_page:: Int
max_items_per_page = 50
