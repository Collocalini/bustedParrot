-----------------------------------------------------------------------------
--
-- Module      :  Post
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

module Post (
-- post_Handler
post_HandlerM
) where

--import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import Control.Monad.State
import Data.Monoid
import qualified Data.Map as Dm
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page_common
import Site_state
import InsertLinks


post_HandlerM :: PostT -> State Routes (Handler App App ())
post_HandlerM p = do
   (Routes {node_map=nm}) <- get
   return $ renderWithSplices "post_base" $ mconcat [
       splicesFrom_post_h p
      ,insertLinks $ Just nm
       ]



splicesFrom_post_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_post_h t = do
  "post_h"  ## return (postT t)





















