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
-- archive_Handler
 archive_HandlerM
,Archive_Handler(..)
) where


------------------------------------------------------------------------------
import qualified Data.Text as T
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

{--
archive_HandlerM :: [PostT] -> State Routes (Handler App App ())
archive_HandlerM p = do
   (Routes {node_map=nm}) <- get

   return $ renderWithSplices "archive_posts"
       (mconcat
       [
       ("posts_h" ## (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ p
       )
       ,insertLinks $ Just nm]
       )
--}

data Archive_Handler =
  Archive_Handler
  {
     posts       :: [PostT]
    ,page_number :: Int
    ,links       :: [String]
    ,routes      :: Routes
  } 


archiveT_HandlerM_common page_number links nm = do
   mconcat $ [
       ("pages" ##
       (I.mapSplices $ I.runChildrenWith . splices_from_page_number page_number)
          $ zip [1..length links] (links)
       )
      ,insertLinks $ Just nm
       ]


archive_HandlerM :: State Archive_Handler (Handler App App ())
archive_HandlerM = do
   (Archive_Handler {
       posts       = p
      ,page_number = page_number
      ,links       = links
      ,routes      = Routes {node_map=nm}
       }) <- get
   

   return $ renderWithSplices "archive_posts"
       $ mconcat $ [
       archiveT_HandlerM_common page_number links nm
       
      ,("posts_h" ##
       (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) p
       )
       
       ]







splices_from_page_number :: Monad n => Int -> (Int, String) -> Splices (I.Splice n)
splices_from_page_number number (page, link)
   |page == number= responce_current_page
   |otherwise = responce_other_page
   where
   responce_current_page = do
       mconcat $ [
         "page_url"        ## I.textSplice $ T.pack $ link
        ,"page_style"        ## I.textSplice $ T.pack $ "page_current"
        ,"page"            ## I.textSplice $ T.pack $ show $ page
        ]
   responce_other_page = do
       mconcat $ [
         "page_url"        ## I.textSplice $ T.pack $ link
        ,"page_style"        ## I.textSplice $ T.pack $ "page"
        ,"page"            ## I.textSplice $ T.pack $ show $ page
        ]
