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
) where


------------------------------------------------------------------------------
--import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
--import System.Directory
--import Data.List
--import Data.Monoid
--import Control.Monad
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page_common







archive_Handler :: [PostT] -> Handler App App ()
archive_Handler p = renderWithSplices "archive/archive_posts"
   (
   "posts_h" ## (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ p
   )


