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
 page_Handler
,pagesT_h_io
,PageT(..)
) where

import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import Control.Monad
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
--import Main_page



data PageT = PageT {
 pageT :: Template
,name :: String
}


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
       (Right s'@(DocumentFile {dfDoc=(TT.HtmlDocument {TT.docContent=docContent})})) <-
                                                        getDoc $ "pages/page" ++ s ++ ".html"
       return $ PageT {pageT=docContent, name=s}

     ff :: [FilePath] -> [String]
     ff fp = map (n . (drop 4)) fp
       where
         n x = (take ((length x)-5) ) x





page_Handler :: PageT -> Handler App App ()
page_Handler p = renderWithSplices "post/post_base"
   (
   splicesFrom_post_h p
   )



splicesFrom_post_h :: Monad n => PageT -> Splices (I.Splice n)
splicesFrom_post_h t = do
  "post_h"  ## return (pageT t)
