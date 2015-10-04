-----------------------------------------------------------------------------
--
-- Module      :  Page
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

module Page (
 --page_Handler
 page_HandlerM
,pagesT_h_io

) where

import qualified Text.XmlHtml as TT
import System.Directory



import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import Control.Monad.State
import qualified Data.Maybe as M
import Data.Monoid
import qualified Data.Map as Dm
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

import Site_state
import Nodes
import InsertLinks
import Page_common





pagesT_h_io :: IO [PageT]
pagesT_h_io = do
   l<- getDirectoryContents "pages"
   mapM s2p $ ff $ filter f l
   where
     f :: FilePath -> Bool
     f ('p':'a':'g':'e':_) = True
     f _ = False

     s2p :: String -> IO PageT
     s2p s = do
       --putStrLn $ "pages/page" ++ s ++ ".html"
       (Right (DocumentFile {dfDoc=(TT.HtmlDocument {TT.docContent=docContent})})) <-
                                                        getDoc $ "pages/page" ++ s ++ ".html"
       return $ PageT {pageT=docContent, name=s}

     ff :: [FilePath] -> [String]
     ff fp = map (n . (drop 4)) fp
       where
         n x = (take ((length x)-5) ) x




page_HandlerM :: PageT -> State Routes (Handler App App ())
page_HandlerM p = do
   (Routes {node_map=nm}) <- get
   return $ renderWithSplices "post_base" $ mconcat
      [
      splicesFrom_page_h p
     ,insertLinks $ Just nm
      ]





splicesFrom_page_h :: Monad n => PageT -> Splices (I.Splice n)
splicesFrom_page_h t = do
  "post_h"  ## return (pageT t)
