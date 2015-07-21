{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Main_page
  (
    main_pageT_Handler
   ,main_pageT_HandlerM
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import Control.Monad.State
--import Control.Monad
--import qualified Page as Pa
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import NodesM
import Main_page_common
import Site_state
import InsertLinks








main_pageT_HandlerM :: [PostT] -> State Routes (Handler App App ())
main_pageT_HandlerM p = do
   (Routes {node_map=nm}) <- get

   return $ renderWithSplices "main_page/main_posts"
       (mconcat
       [(
       (splicesFrom_main_postsT_h) $ head p
       ),
       ("posts_h" ##
       {-(I.mapSplices $ I.runChildrenWith . (\f -> splicesFrom_main_postsT_h_M f nm))
          $ take 9 $ tail p-}
       (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ take 9 $ tail p

       )
       ,insertLinks $ Just nm]
       )




main_pageT_Handler :: [PostT] -> Handler App App ()
main_pageT_Handler p = renderWithSplices "main_page/main_posts"
   (mconcat
   [(
   (splicesFrom_main_postsT_h) $ head p
   ),
   ("posts_h" ##
   (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ take 9 $ tail p
   )]
   )











