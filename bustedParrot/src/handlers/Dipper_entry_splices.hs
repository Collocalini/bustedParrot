-----------------------------------------------------------------------------
--
-- Module      :  Dipper_entry_splices
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

module Dipper_entry_splices (
 splicesFrom_dippers_common
,splicesFrom_dippers
,splicesFrom_dippers_entry_case
,splicesFrom_main_page_dippers_entry_case
,splicesFrom_dippers_entry_case_tags
,splicesFrom_individual_dipper_case
,splicesFrom_individual_dipper_case_tags


) where

import Dipper_common
import qualified Data.Text as T
import Data.Monoid
import           Heist
import qualified Heist.Interpreted as I
import qualified Data.Maybe as M
import Nodes





splicesFrom_dippers_common :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers_common t = do
   mconcat $ [
     "dipper_url"        ## I.textSplice $ url t
    ,"dipper_url_img"    ## dipper_url_img_case
    ,"dipper_miniature"  ## I.textSplice $ M.fromJust $ miniature t
    ,"image_style"       ## image_style_case
    ,"auto_caption"      ## auto_caption_case
    ]
   where
   dipper_url_img_case
      |link_is_local (url t) =  I.textSplice $ url t
      |otherwise             =  I.textSplice $ M.fromJust $ miniature t

   image_style_case
      |(isVertical t)              = I.textSplice $ "img_fit_height"
      |not $ link_is_local (url t) = I.textSplice $ "img_miniature"
      |otherwise                   = I.textSplice $ "img_fit_width"

   auto_caption_case
      |link_is_local (url t) =  I.textSplice $ ""
      |otherwise             =  I.textSplice $ "Нажми для полного размера / Click for full size"




splicesFrom_dippers :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers t = do
   mconcat $ [
    "page_url"          ## I.textSplice $ individual_dipper_node_link' $ T.unpack $ page_url t
    ,splicesFrom_dippers_common t
    ]

splicesFrom_main_page_dippers_entry_case :: Monad n =>  Dipper -> Splices (I.Splice n)
splicesFrom_main_page_dippers_entry_case t = do
   splicesFrom_case_common "main_dipper_entry_img" "main_dipper_entry_obj" t $ splicesFrom_dippers t

splicesFrom_dippers_entry_case :: Monad n =>  Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers t

splicesFrom_dippers_entry_case_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case_tags s t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_dippers_entry_case_common t sfd =
   splicesFrom_case_common "dipper_entry_img" "dipper_entry_obj" t sfd

splicesFrom_individual_dipper_case :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers t

splicesFrom_individual_dipper_case_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case_tags s t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_individual_dipper_case_common t sfd =
   splicesFrom_case_common "individual_dipper_entry_img"
                           "individual_dipper_entry_obj" t sfd



splicesFrom_case_common tp_img tp_obj t sfd = do
   "entry" ## dipper_entry_img_obj tp_img tp_obj t sfd


dipper_entry_img_obj tp_img tp_obj (Dipper {miniature = Just m}) t = do
   case (node_is_an_svg m) of
      True  -> I.callTemplate tp_obj t
      False -> I.callTemplate tp_img t

dipper_entry_img_obj tp_img _ (Dipper {miniature = Nothing}) t = do
   I.callTemplate tp_img t



splicesFrom_dippers_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_tags tag t = do
   mconcat $ [
    "page_url"          ## I.textSplice $ individual_dipper_tagged_page_link' t tag
    ,splicesFrom_dippers_common t
    ]
