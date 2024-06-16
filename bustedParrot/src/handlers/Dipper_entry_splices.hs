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
    ,"links"             ## fillInAltLinks $ altUrls t
    ,"dipper_title"      ## I.textSplice $ ""
    ]
   where
   dipper_url_img_case
      |(dipperDisplayOnPageCase t) == DopUseURL       = I.textSplice $ url t
      |(dipperDisplayOnPageCase t) == DopUseMiniature = I.textSplice $ M.fromJust $ miniature t
      |otherwise                                      = I.textSplice $ M.fromJust $ miniature t

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
      |(dipperType t) == DtPeerTubeVideoEmbed = I.textSplice $ ""
      |otherwise             =  I.textSplice $ "Нажми для полного размера / Click for full size"


   
   
   
dipperDisplayOnPageCase t
  |link_is_local (url t)           =  DopUseURL
  |node_is_a_peerTubeEmbed (url t) =  DopUseURL
  |otherwise                       =  DopUseMiniature




--fillInAltLinks :: Monad n => [T.Text] -> Splices (I.Splice n)
fillInAltLinks l = (I.mapSplices $ I.runChildrenWith . (\u-> "link" ## I.textSplice u)) l
       


splicesFrom_dippers :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers t = do
   mconcat $ [
    "page_url"          ## I.textSplice $ individual_dipper_node_link' $ T.unpack $ page_url t
    ,splicesFrom_dippers_common t
    ]

splicesFrom_main_page_dippers_entry_case :: Monad n =>  Dipper -> Splices (I.Splice n)
splicesFrom_main_page_dippers_entry_case t@(Dipper {miniatureType = dt}) = do
   splicesFrom_case_common [(DtNotDefined,  "main_dipper_entry_img")
                           ,(DtRasterImage, "main_dipper_entry_img")
                           ,(DtSvgImage, "main_dipper_entry_obj")
                           ,(DtMp4Video, "")
                           ,(DtPeerTubeVideoEmbed, "")
                           ,(DtHtmlCode, "")
                           ]  dt $ splicesFrom_dippers t

splicesFrom_dippers_entry_case :: Monad n =>  Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers t

splicesFrom_dippers_entry_case_tags :: Monad n => T.Text -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_entry_case_tags s t = do
   splicesFrom_dippers_entry_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_dippers_entry_case_common (Dipper {miniatureType = dt}) sfd =
   splicesFrom_case_common [(DtNotDefined,  "dipper_entry_img")
                           ,(DtRasterImage, "dipper_entry_img")
                           ,(DtSvgImage, "dipper_entry_obj")
                           ,(DtMp4Video, "")
                           ,(DtPeerTubeVideoEmbed, "")
                           ,(DtHtmlCode, "")
                           ]  dt sfd

splicesFrom_individual_dipper_case :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers t

splicesFrom_individual_dipper_case_tags :: Monad n => T.Text -> Dipper -> Splices (I.Splice n)
splicesFrom_individual_dipper_case_tags s t =
   splicesFrom_individual_dipper_case_common t $ splicesFrom_dippers_tags s t

splicesFrom_individual_dipper_case_common t@(Dipper {dipperType = dt
                                                  ,miniatureType = mt}) 
                                          sfd 
   |(dipperDisplayOnPageCase t) == DopUseURL       = splicesFrom_case_common dtMap dt sfd
   |(dipperDisplayOnPageCase t) == DopUseMiniature = splicesFrom_case_common dtMap mt sfd
   |otherwise = splicesFrom_case_common dtMap mt sfd
   where
   dtMap = [(DtNotDefined,  "individual_dipper_entry_img")
           ,(DtRasterImage, "individual_dipper_entry_img")
           ,(DtSvgImage,    "individual_dipper_entry_obj")
           ,(DtMp4Video,    "individual_dipper_entry_video")
           ,(DtPeerTubeVideoEmbed, "individual_dipper_entry_peerTubeEmbed")
           ,(DtHtmlCode, "")
           ]



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



--(Dipper {dipperType = dt})

dipper_entry_case_type m dt t = do
   I.callTemplate (M.fromJust $ Dmap.lookup dt m) t







splicesFrom_dippers_tags :: Monad n => T.Text -> Dipper -> Splices (I.Splice n)
splicesFrom_dippers_tags tag t = do
   mconcat $ [
    "page_url"          ## I.textSplice $ individual_dipper_tagged_page_link' t tag
    ,splicesFrom_dippers_common t
    ]
