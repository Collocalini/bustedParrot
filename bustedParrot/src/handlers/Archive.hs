-----------------------------------------------------------------------------
--
-- Module      :  Archive
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

module Archive (
 archive_Handler
,archive_HandlerM
) where


------------------------------------------------------------------------------
--import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import Control.Monad.State
import Data.Monoid
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page_common
import Site_state
import InsertLinks


archive_HandlerM :: [PostT] -> State Routes (Handler App App ())
archive_HandlerM p = do
   (Routes {node_map=nm}) <- get

   return $ renderWithSplices "archive/archive_posts"
       (mconcat
       [
       ("posts_h" ## (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ p
       )
       ,insertLinks $ Just nm]
       )



archive_Handler :: [PostT] -> Handler App App ()
archive_Handler p = renderWithSplices "archive/archive_posts"
   (
   "posts_h" ## (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ p
   )


