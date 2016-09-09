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
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq


data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,url_raw :: T.Text
 ,comment :: Maybe T.Text
 ,isVertical :: Bool
 ,scale :: DipperScale
 ,dipperType :: DipperType
 ,miniatureType :: DipperType
} deriving (Show,Eq,Generic, NFData)

type Dippers = [Dipper]


data DipperScale =
                  NotDefined
                 |FitWidth
                 |FitHeight
                 |FitSmallScreens
                 |FitPostPreview
                 |LikeTagImage
                 |ThumbnailSize
                 |AsIs
                 deriving (Show,Eq,Generic, NFData)


data DipperType = NotDefined
                 |RasterImage
                 |SvgImage
                 |Mp4Video
                 |HtmlCode














