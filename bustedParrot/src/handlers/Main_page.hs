{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Main_page
  (
    --main_pageT_Handler
   main_pageT_HandlerM
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
import Dipper_entry_splices







main_pageT_HandlerM :: [PostT] -> State Routes (Handler App App ())
main_pageT_HandlerM p = do
   (Routes {node_map=nm, dippersT=d}) <- get

   return $ renderWithSplices "main_posts"
       (mconcat
       [
        ("dippers" ##
        (I.mapSplices $ I.runChildrenWith . splicesFrom_main_page_dippers_entry_case) $ take 8 d
        )

       ,(
        (splicesFrom_main_postsT_h) $ head p
        )

       ,("posts_h" ##
        (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ take 9 $ tail p
        )
       ,insertLinks $ Just nm]
       )












