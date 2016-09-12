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
import qualified Data.Map as Dmap
import qualified Data.ByteString as B



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
      |  (isVertical t)
       &&(not ((scale t)==DsAsIs))
       &&(link_is_local (url t)) = I.textSplice $ "img_fit_height"

      |  ((scale t)==DsAsIs)
       &&(link_is_local (url t)) = I.textSplice $ "img_as_is"

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
   splicesFrom_case_common [(DtNotDefined,  "main_dipper_entry_img")
                           ,(DtRasterImage, "main_dipper_entry_img")
                           ,(DtSvgImage, "main_dipper_entry_obj")
                           ,(DtMp4Video, "")
                           ,(DtHtmlCode, "")
                           ]  t $ splicesFrom_dippers t

splicesFrom_dippers_entry_case :: Monad n =>  Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers t

splicesFrom_dippers_entry_case_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case_tags s t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_dippers_entry_case_common t sfd =
   splicesFrom_case_common [(DtNotDefined,  "dipper_entry_img")
                           ,(DtRasterImage, "dipper_entry_img")
                           ,(DtSvgImage, "dipper_entry_obj")
                           ,(DtMp4Video, "")
                           ,(DtHtmlCode, "")
                           ]  t sfd

splicesFrom_individual_dipper_case :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers t

splicesFrom_individual_dipper_case_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case_tags s t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_individual_dipper_case_common t sfd =
   splicesFrom_case_common [(DtNotDefined,  "individual_dipper_entry_img")
                           ,(DtRasterImage, "individual_dipper_entry_img")
                           ,(DtSvgImage, "individual_dipper_entry_obj")
                           ,(DtMp4Video, "individual_dipper_entry_video")
                           ,(DtHtmlCode, "")
                           ] t sfd


{-splicesFrom_case_common :: Monad n => [(DipperType, B.ByteString)] 
                           -> Dipper 
                           -> Splices (I.Splice n) 
                           -> Splices (I.Splice n)-}
splicesFrom_case_common m t sfd = do
   "entry" ## dipper_entry_case_type (Dmap.fromList m) t sfd



{-----------------dipper_entry_img_obj----------------------
dipper_entry_img_obj tp_img tp_obj (Dipper {miniature = Just m}) t = do
   case (node_is_a_svg m) of
      True  -> I.callTemplate tp_obj t
      False -> I.callTemplate tp_img t

dipper_entry_img_obj tp_img _ (Dipper {miniature = Nothing}) t = do
   I.callTemplate tp_img t
-------------------------------------------------------------}


{-dipper_entry_case_type:: Monad n => Dmap.Map DipperType B.ByteString 
                           -> Dipper 
                           -> Splices (I.Splice n) 
                           -> HeistT n n Template-}
dipper_entry_case_type m (Dipper {dipperType = dt}) t = do
   I.callTemplate (M.fromJust $ Dmap.lookup dt m) t







splicesFrom_dippers_tags :: Monad n => String -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_tags tag t = do
   mconcat $ [
    "page_url"          ## I.textSplice $ individual_dipper_tagged_page_link' t tag
    ,splicesFrom_dippers_common t
    ]
