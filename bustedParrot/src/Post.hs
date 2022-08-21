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
,post_HandlerM_tagged
,post_HandlerM_tagged_parented
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
import Nodes (post_node_link', post_node_link_tagged'')


post_HandlerM :: PostT -> State Routes (Handler App App ())
post_HandlerM p = post_HandlerM_common p Nothing Nothing
       
post_HandlerM_tagged :: PostT -> String -> State Routes (Handler App App ())
post_HandlerM_tagged p tags = post_HandlerM_common p (Just tags) Nothing

post_HandlerM_tagged_parented :: PostT -> String -> String -> State Routes (Handler App App ())
post_HandlerM_tagged_parented p tags parent = post_HandlerM_common p (Just tags) (Just parent)

post_HandlerM_common :: PostT -> Maybe String -> Maybe String -> State Routes (Handler App App ())
post_HandlerM_common p tags parent_url = do
   (Routes {node_map=nm}) <- get
   return $ renderWithSplices "post_base" $ mconcat [
       postT_parent_no_parent parent_url
      ,postTIndividualPageNavigation p tags
      ,splicesFrom_post_h p
      ,insertLinks $ Just nm
       ]
   
   where
   postTIndividualPageNavigation p Nothing = postT_individual_page_navigation p
   postTIndividualPageNavigation p (Just t) = postT_individual_page_navigation_tagged p t
   


splicesFrom_post_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_post_h t = do
  "post_h"  ## return (postT t)






postT_individual_page_navigation :: Monad n => PostT -> Splices (I.Splice n)
postT_individual_page_navigation (PostT {next=n, prev=p}) = do
   mconcat $ [
     "nav_prev"   ## postT_individual_page_nav_no_nav p $ postT_individual_page_nav_prev p
    ,"nav_next"   ## postT_individual_page_nav_no_nav n $ postT_individual_page_nav_next n
    ]



postT_individual_page_navigation_tagged :: Monad n => PostT -> String -> Splices (I.Splice n)
postT_individual_page_navigation_tagged (PostT {next=n, prev=p}) tag = do
   mconcat $ [
     "nav_prev"   ## postT_individual_page_nav_no_nav p $ postT_individual_page_nav_prev_tagged p tag
    ,"nav_next"   ## postT_individual_page_nav_no_nav n $ postT_individual_page_nav_next_tagged n tag
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
   
   
   
   
postT_individual_page_nav_common_tagged :: Monad n => Maybe Int -> String -> Splices (I.Splice n)
postT_individual_page_nav_common_tagged d tag = do
   mconcat $ [
     "url"        ## I.textSplice $ nav_url d
    ,"style"      ## I.textSplice $ nav_style d
    ]
   where
   nav_url :: Maybe Int -> T.Text
   nav_url (Just x) = post_node_link_tagged'' x tag
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


postT_individual_page_nav_prev_tagged :: Monad n => Maybe Int -> String -> Splices (I.Splice n)
postT_individual_page_nav_prev_tagged d tags = do
   mconcat $ [
     postT_individual_page_nav_common_tagged d tags
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_prev" (return ())
    ]


postT_individual_page_nav_next_tagged :: Monad n => Maybe Int -> String -> Splices (I.Splice n)
postT_individual_page_nav_next_tagged d tags = do
   mconcat $ [
     postT_individual_page_nav_common_tagged d tags
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_next" (return ())
    ]



postT_individual_page_nav_no_nav (Just d) t = do
   I.callTemplate "dipper_individual_page_nav" t

postT_individual_page_nav_no_nav Nothing t = do
   I.callTemplate "dipper_individual_page_no_nav" t


postT_parent :: Monad n => String -> Splices (I.Splice n)
postT_parent parent_url = do
  mconcat $ [
   "parent_url" ## I.textSplice $ T.pack parent_url
   ]

postT_parent_no_parent (Just parent_url) = do
   "nav_parent" ## I.callTemplate "post_nav_to_parent" $ postT_parent parent_url

postT_parent_no_parent Nothing = do
   "nav_parent" ## I.callTemplate "post_nav_no_parent" (return ())






























