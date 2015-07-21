-----------------------------------------------------------------------------
--
-- Module      :  NodesM
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
{-# LANGUAGE OverloadedStrings #-}

module NodesM (
 node_to_linkM
 --,insertLinks
) where


import Control.Monad.State
import qualified Data.Map as Dm
--import qualified Data.Maybe as M
import qualified Data.Text as T
import Nodes
import Site_state
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import Control.Monad.State
import qualified Data.Maybe as M
import Data.Monoid
import qualified Data.Map as Dm




node_to_linkM :: T.Text -> State Routes T.Text
node_to_linkM n = do
   (Routes {node_map=nm}) <- get
   return $ node_to_link n nm






