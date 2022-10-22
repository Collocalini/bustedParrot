-----------------------------------------------------------------------------
--
-- Module      :  Dipper_common
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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}



module Dipper_common (
 Dipper(..)
,Dippers
,DipperScale(..)
,DipperType(..)
,DipperDisplayOnPage(..)
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq


data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,altUrls :: [T.Text]
 ,url_raw :: T.Text
 ,altUrls_raw :: [T.Text]
 ,comment :: Maybe T.Text
 ,isVertical :: Bool
 ,scale :: DipperScale
 ,dipperType :: DipperType
 ,miniatureType :: DipperType
} deriving (Show,Eq,Generic, NFData)

type Dippers = [Dipper]


data DipperScale =
                  DsNotDefined
                 |DsFitWidth
                 |DsFitHeight
                 |DsFitSmallScreens
                 |DsFitPostPreview
                 |DsLikeTagImage
                 |DsThumbnailSize
                 |DsAsIs
                 deriving (Show,Eq,Generic, NFData)


data DipperType = DtNotDefined
                 |DtRasterImage
                 |DtSvgImage
                 |DtMp4Video
                 |DtPeerTubeVideoEmbed
                 |DtHtmlCode
                 deriving (Show,Eq,Ord, Generic, NFData)


data DipperDisplayOnPage = 
                  DopNotDefined
                 |DopUseURL
                 |DopUseMiniature
                 deriving (Show,Eq,Ord, Generic, NFData)











