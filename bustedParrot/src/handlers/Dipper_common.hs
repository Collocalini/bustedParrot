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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass ,OverloadedStrings #-}



module Dipper_common where
{-
module Dipper_common (
 Dipper(..)
,Dippers
,DipperScale(..)
,DipperType(..)
,DipperDisplayOnPage(..)
) where
-}
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B8
import Control.Monad 
import Data.Maybe

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



instance FromJSON Dipper where
  parseJSON = withObject "Dipper" $ \o -> do
    
    miniature  <- o .:? "miniature"
    name       <- o .:? "name"
    page_url   <- o .:  "page_url"
    url        <- o .:  "url"
    altUrls    <- o .:? "altUrls" .!= []
    url_raw    <- o .: "url_raw"
    altUrls_raw  <- o .:? "altUrls_raw" .!= []
    comment      <- o .:? "comment"
    isVertical   <- o .:  "isVertical"
    scale        <- o .:? "scale" .!= DsNotDefined
    dipperType   <- o .:? "dipperType" .!= DtNotDefined
    miniatureType <- o .:? "miniatureType" .!= DtNotDefined
        
    return 
      $ Dipper 
          miniature 
          name      
          page_url  
          url       
          altUrls  
          url_raw   
          altUrls_raw 
          comment      
          isVertical   
          scale        
          dipperType   
          miniatureType 
      

instance ToJSON Dipper where
  toJSON  (Dipper 
            miniature 
            name      
            page_url  
            url       
            altUrls  
            url_raw   
            altUrls_raw 
            comment      
            isVertical   
            scale        
            dipperType   
            miniatureType 
          ) = 
          object
            $ concat 
              [
                [--"miniature" .= miniature
                -- ,"name"      .= name
                 "page_url"  .= page_url
                ,"url"       .= url
                ,"altUrls"   .= altUrls
                ,"url_raw"   .= url_raw
                ,"altUrls_raw" .= altUrls_raw
              --  ,"comment"     .= comment
                ,"isVertical"  .= isVertical
                ,"scale"       .= scale
                ,"dipperType"  .= dipperType
                ,"miniatureType" .= miniatureType
                ]
                ,map ("miniature" .= ) $ maybeToList miniature
                ,map ("name" .= ) $ maybeToList name
                ,map ("comment" .= ) $ maybeToList comment
              ]





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


instance FromJSON DipperScale where
  parseJSON = withText "DipperScale" $ step1
    where

    step1 "DsNotDefined" = return DsNotDefined
    step1 "DsFitWidth" = return DsFitWidth
    step1 "DsFitHeight" = return DsFitHeight
    step1 "DsFitSmallScreens" = return DsFitSmallScreens
    step1 "DsFitPostPreview" = return DsFitPostPreview
    step1 "DsLikeTagImage" = return DsLikeTagImage
    step1 "DsThumbnailSize" = return DsThumbnailSize
    step1 "DsAsIs" = return DsAsIs
    step1 _ = mzero

instance ToJSON DipperScale where
  toJSON DsNotDefined = String "DsNotDefined"
  toJSON DsFitWidth = String "DsFitWidth"
  toJSON DsFitHeight = String "DsFitHeight"
  toJSON DsFitSmallScreens = String "DsFitSmallScreens"
  toJSON DsFitPostPreview = String "DsFitPostPreview"
  toJSON DsLikeTagImage = String "DsLikeTagImage"
  toJSON DsThumbnailSize = String "DsThumbnailSize"
  toJSON DsAsIs = String "DsAsIs"


data DipperType = DtNotDefined
                 |DtRasterImage
                 |DtSvgImage
                 |DtMp4Video
                 |DtPeerTubeVideoEmbed
                 |DtHtmlCode
                 deriving (Show,Eq,Ord, Generic, NFData)

instance FromJSON DipperType where
  parseJSON = withText "DipperType" $ step1
    where

    step1 "DtNotDefined" = return DtNotDefined
    step1 "DtRasterImage" = return DtRasterImage
    step1 "DtSvgImage" = return DtSvgImage
    step1 "DtMp4Video" = return DtMp4Video
    step1 "DtPeerTubeVideoEmbed" = return DtPeerTubeVideoEmbed
    step1 "DtHtmlCode" = return DtHtmlCode
    step1 _ = mzero

instance ToJSON DipperType where
  toJSON DtNotDefined = String "DtNotDefined"
  toJSON DtRasterImage = String "DtRasterImage"
  toJSON DtSvgImage = String "DtSvgImage"
  toJSON DtMp4Video = String "DtMp4Video"
  toJSON DtPeerTubeVideoEmbed = String "DtPeerTubeVideoEmbed"
  toJSON DtHtmlCode = String "DtHtmlCode"


data DipperDisplayOnPage = 
                  DopNotDefined
                 |DopUseURL
                 |DopUseMiniature
                 deriving (Show,Eq,Ord, Generic, NFData)


{-
instance FromJSON  where
  parseJSON = withText "" $ step1
    where

    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 "" = return 
    step1 _ = mzero

instance ToJSON  where
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""
  toJSON  = String ""


-}




