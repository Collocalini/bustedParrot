-----------------------------------------------------------------------------
--
-- Module      :  Dipper
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
{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Dipper_image (
--  dipper_check_orientation
  loadImage
 ,image_is_vertical
 ,deduceRepresentationScale
 ,representationScaleGrading
 ,deduceDipperType
) where


import qualified Codec.Picture as CPic
import qualified System.FilePath.Posix as Fp
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.DeepSeq
import qualified Text.XmlHtml as X
import Data.Maybe
import Control.Exception
import Data.List

import Dipper_common
import Nodes




{-
dipper_check_orientation :: Dipper -> IO Dipper
dipper_check_orientation d
   |link_is_local $ url d = (loadImage $ T.unpack $ url d) >>= ck
   |otherwise = return $!! (\de -> de {isVertical=False}) d

   where
     ck Nothing    = return $!! (\de -> de {isVertical=False}) d
     ck (Just img) = do
       {-putStrLn "!!!!!!!!!!!!!!!!------!!!!!!!!!!!!!!!!!!!!!"
       print $ url d
       print $ image_is_vertical img
       putStrLn "!!!!!!!!!!!!!!!!------!!!!!!!!!!!!!!!!!!!!!"-}
       
       return $!! (\de -> de {isVertical=image_is_vertical img}) d
-}


{-- ================================================================================================
================================================================================================ --}
loadImage :: FilePath -> IO (Maybe (Either CPic.DynamicImage X.Document))
loadImage name = do
  tryToLoad $ caseLoad name
  {-case t of
    Just t->do
      putStrLn "!!!!!!!!!!!!!!!!-=-=-=-=-!!!!!!!!!!!!!!!!!!!!!"
      print name
      print $ image_is_vertical t
      putStrLn "!!!!!!!!!!!!!!!!-=-=-=-=-!!!!!!!!!!!!!!!!!!!!!"
    _ -> do
      putStrLn "!!!!!!!!!!!!!!!!-=-skip-=-!!!!!!!!!!!!!!!!!!!!!"
      print name
      putStrLn "!!!!!!!!!!!!!!!!-=-skip-=-!!!!!!!!!!!!!!!!!!!!!"
  return t-}
  where
    tryToLoad [] = return Nothing
    tryToLoad tryOrder@(h:rest) = do
      l<- h
      case l of
        Nothing-> tryToLoad rest
        (Just x)-> h
    
    
    caseLoad name
      |(Fp.takeExtension name)==".svg" = [loadSvg name, loadWithCPic name]
      |otherwise = [loadWithCPic name, loadSvg name]
        
----------------------------------------------------------------------------------------------------

loadWithCPic :: FilePath -> IO (Maybe (Either CPic.DynamicImage X.Document))
loadWithCPic name =
      do image <- CPic.readImage $ Fp.normalise ("./" ++ name)
         case image of
           (Left s) -> do
                         print s
                         return Nothing

           (Right d) ->
                      do
                         return $ Just $ Left d


loadSvg :: FilePath -> IO (Maybe (Either CPic.DynamicImage X.Document))
loadSvg f =  do
  s<- try $ B.readFile $ Fp.normalise ("./" ++ f)
  case s of
    Left (e :: SomeException) -> return Nothing 
    Right s -> do
      case (X.parseXML f s) of
        Left s -> return Nothing
        Right d@(X.XmlDocument {X.docContent=s}) -> return $ Just $ Right d

--loadMp4 :: 




image_is_vertical :: Either CPic.DynamicImage X.Document -> Bool
image_is_vertical  (Left (CPic.ImageRGB8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = h > w

image_is_vertical  (Left (CPic.ImageRGBA8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = h > w

image_is_vertical  (Left (CPic.ImageYCbCr8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = h > w
image_is_vertical (Right (X.XmlDocument
                          {X.docContent=s})
                  ) = (\(w,h)-> h > w) $ fromMaybe (0,0) $ svgWH s

image_is_vertical  _ = False



--svgWH [] = Nothing
svgWH :: [X.Node] -> Maybe (Float, Float)
svgWH nodes = wh $ find (\x->(X.tagName x)==(Just "svg")) nodes
  where
    wh  Nothing          = Nothing
    wh  (Just n)         = wh' (X.getAttribute "width" n, X.getAttribute "height" n)
    wh' (Just w, Just h) = Just (read $ T.unpack w, read $ T.unpack h)
    wh' _                = Nothing
        
        



representationScaleGrading w h
  |     (w<=256)&&(h<=256) = DsAsIs
 -- |not ((w<=256)&&(h<=256)) =
 -- |
  |otherwise = DsNotDefined


deduceRepresentationScale :: Either CPic.DynamicImage X.Document -> DipperScale
deduceRepresentationScale (Left (CPic.ImageRGB8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = representationScaleGrading w h

deduceRepresentationScale  (Left (CPic.ImageRGBA8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = representationScaleGrading w h

deduceRepresentationScale  (Left (CPic.ImageYCbCr8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     }))
                   ) = representationScaleGrading w h
deduceRepresentationScale (Right (X.XmlDocument
                          {X.docContent=s})
                  ) = (\(w,h)-> representationScaleGrading w h)
                                            $ fromMaybe (0,0) $ svgWH s

deduceRepresentationScale  _ = DsNotDefined





deduceDipperType :: FilePath -> DipperType
deduceDipperType f
  |node_is_a_raster $ T.pack f = DtRasterImage
  |node_is_a_svg $ T.pack f = DtSvgImage
  |node_is_a_mp4 $ T.pack f = DtMp4Video
  |otherwise = DtNotDefined
  
  
  
{-  
deduceDipperType :: Either CPic.DynamicImage X.Document -> DipperType
deduceDipperType f
  |node_is_a_raster $ T.pack f = DtRasterImage
  |node_is_a_svg $ T.pack f = DtSvgImage
  |otherwise = DtNotDefined  
  -}
  
  
  


