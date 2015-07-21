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
import qualified Page as Pa
--import qualified Archive as A
import qualified Dipper as D
import qualified Nodes as N

data Routes = Routes {
 postsT :: [MP.PostT]
,pagesT :: [Pa.PageT]
,dippersT :: D.Dippers
,dippers_references :: [(D.Dipper,[MP.PostT])]
,dippers_tags :: [(D.Dipper,[String])]
,node_map :: N.Node_map
}

max_items_per_page:: Int
max_items_per_page = 50
