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

import qualified Data.Text as T
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
import Nodes (post_node_link')


post_HandlerM :: PostT -> State Routes (Handler App App ())
post_HandlerM p = do
   (Routes {node_map=nm}) <- get
   return $ renderWithSplices "post_base" $ mconcat [
       postT_individual_page_navigation p
      ,splicesFrom_post_h p
      ,insertLinks $ Just nm
       ]



splicesFrom_post_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_post_h t = do
  "post_h"  ## return (postT t)




postT_individual_page_navigation :: Monad n => PostT -> Splices (I.Splice n)
postT_individual_page_navigation (PostT {next=n, prev=p}) = do
   mconcat $ [
     "nav_prev"   ## postT_individual_page_nav_no_nav p $ postT_individual_page_nav_prev p
    ,"nav_next"   ## postT_individual_page_nav_no_nav n $ postT_individual_page_nav_next n
    ]



postT_individual_page_nav_common :: Monad n => Maybe Int -> Splices (I.Splice n)
postT_individual_page_nav_common d = do
   mconcat $ [
     "url"        ## I.textSplice $ nav_url d
    ,"style"      ## I.textSplice $ nav_style d
    ]
   where
   nav_url :: Maybe Int -> T.Text
   nav_url (Just x) = post_node_link' x
   nav_url Nothing = ""

   nav_style (Just _ ) = "dipper_navig"
   nav_style Nothing   = "dipper_no_navig"



postT_individual_page_nav_prev :: Monad n => Maybe Int -> Splices (I.Splice n)
postT_individual_page_nav_prev d = do
   mconcat $ [
     postT_individual_page_nav_common d
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_prev" (return ())
    ]


postT_individual_page_nav_next :: Monad n => Maybe Int -> Splices (I.Splice n)
postT_individual_page_nav_next d = do
   mconcat $ [
     postT_individual_page_nav_common d
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_next" (return ())
    ]



postT_individual_page_nav_no_nav (Just d) t = do
   I.callTemplate "dipper_individual_page_nav" t

postT_individual_page_nav_no_nav Nothing t = do
   I.callTemplate "dipper_individual_page_no_nav" t































