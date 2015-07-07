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
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Dipper (
  Dipper(..)
 ,Dippers
 ,dippersT_io
 ,dippersT_Handler
 ,give_dippers_references
 ,dipperT_individual_page_Handler
) where

import Data.Aeson
import GHC.Generics
import qualified Data.Maybe as M
import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import qualified System.FilePath.Posix as Fp
import Data.List
--import qualified Data.Maybe as DM
import Data.Monoid
--import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Codec.Picture as CPic

------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page










data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,comment :: Maybe T.Text
 ,isVertical :: Bool
} deriving (Show)


data Dipper_json = Dipper_json {
  miniature_json :: T.Text
 ,name_json :: T.Text
 ,url_json :: T.Text
 ,comment_json :: T.Text
} deriving (Show,Generic)


type Dippers_json = [Dipper_json]
type Dippers = [Dipper]


instance FromJSON Dipper_json
instance ToJSON Dipper_json





dippersT_io :: IO Dippers
dippersT_io = do
   l<- getDirectoryContents "dippers"
   d <- mapM number2dipper $ number_from_dipper_name $ filter isDipperFile l
   mapM dipper_check_orientation $ concat d
   {--mapM number2post $ reverse $ sort $ number_from_post_name $ filter isPostFile l
   where
     f :: FilePath -> Bool
     f ('d':'i':'p':'p':'e':'r':'_':_) = True
     f _ = False

     s2p :: String -> IO Dippers
     s2p s = do
       d <- eitherDecode <$> B.readFile ("dippers/dipper_" ++ s ++ ".json"):: IO (Either String Dippers_json)
       case d of
        Left err -> do putStrLn err
                       return []
        Right dl -> return $ M.catMaybes $ map (give_dipper s) dl


     ff :: [FilePath] -> [String]
     ff fp = map (n . (drop 7)) fp
       where
         n x = (take ((length x)-5) ) x
--}


isDipperFile :: FilePath -> Bool
isDipperFile ('d':'i':'p':'p':'e':'r':'_':_) = True
isDipperFile _ = False



number2dipper :: String -> IO Dippers
number2dipper s = do
   d <- eitherDecode <$> B.readFile ("dippers/dipper_" ++ s ++ ".json"):: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map (give_dipper s) dl


number_from_dipper_name :: [FilePath] -> [String]
number_from_dipper_name fp = map (n . (drop 7)) fp
       where
         n x = (take ((length x)-5) ) x

{-
-}




give_dipper :: String -> Dipper_json -> Maybe Dipper
give_dipper _   (Dipper_json {miniature_json = _
                             ,name_json      = _
                             ,url_json       = ""
                             ,comment_json   = _
                             }) = Nothing

give_dipper _   (Dipper_json {miniature_json = m
                             ,name_json      = n
                             ,url_json       = u
                             ,comment_json   = c
                             }) =
   Just $ Dipper {
     miniature = maybe_miniature
    ,name      = maybe_name
    ,page_url  = T.pack $ Fp.replaceExtension (Fp.takeFileName $ T.unpack u) ".html"
    ,url       = u
    ,comment   = maybe_comment
    ,isVertical = False
    }
   where
   maybe_miniature
     |m == "" = Nothing
     |otherwise = Just m

   maybe_name
     |n == "" = Nothing
     |otherwise = Just n

   maybe_comment
     |c == "" = Nothing
     |otherwise = Just c




dipper_check_orientation :: Dipper -> IO Dipper
dipper_check_orientation d
   |link_is_local $ url d = do
       img <- loadImage $ T.unpack $ url d
       case img of
          Nothing  -> ioError (userError "image loading error")
          Just img -> return $ (\de -> de {isVertical=image_is_vertical img}) d

   |otherwise = return $ (\de -> de {isVertical=False}) d




{-- ================================================================================================
================================================================================================ --}
loadImage :: FilePath -> IO (Maybe (CPic.DynamicImage))
loadImage name = do image <- CPic.readImage $ Fp.normalise ("./" ++ name)
                    case image of
                      (Left s) -> do
                                    print s
                                    return Nothing

                      (Right d) ->
                                 do
                                    return $ Just d
----------------------------------------------------------------------------------------------------

image_is_vertical :: CPic.DynamicImage -> Bool
image_is_vertical  (CPic.ImageRGB8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     })
                   ) = h > w

image_is_vertical  (CPic.ImageRGBA8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     })
                   ) = h > w

image_is_vertical  (CPic.ImageYCbCr8
                     (CPic.Image {
                        CPic.imageWidth = w
                       ,CPic.imageHeight = h
                     })
                   ) = h > w

image_is_vertical  _ = False






give_all_posts :: IO [(String, T.Text)]
give_all_posts = do
   l<- getDirectoryContents "posts"
   let files = sort $ filter isPostFile l
   r<- mapM (\n-> readFile ("posts/" ++ n)) files
   return $ zip files $ map T.pack r




dipper_is_found_in :: [(String, T.Text)] -> Dipper -> [String]
dipper_is_found_in posts
   (Dipper {
     miniature = _
    ,name      = _
    ,page_url  = _
    ,url       = u
    ,comment   = _
    })  = (\(l,_) -> l) $ unzip $ filter (\(_,t) -> T.isInfixOf u t) posts

dippers_references :: Dippers -> [(String, T.Text)] -> [[String]]
dippers_references d posts = map (dipper_is_found_in posts) d




give_dippers_references :: IO [(Dipper,[PostT])]
give_dippers_references = do
   d   <- dippersT_io
   ps  <- give_all_posts
   pTs <- mapM step1 $ dippers_references d ps
   return $ zip d pTs
   where
   step1 :: [String] -> IO [PostT]
   step1 s = mapM number2post $ reverse $ sort $ number_from_post_name s



{-
give_dippers_tags :: IO [(Dipper,[PostT])]
give_dippers_tags = do
   d   <- dippersT_io
   ps  <- give_all_posts
   pTs <- mapM step1 $ dippers_references d ps
   return $ zip d pTs
   where
   step1 :: [String] -> IO [PostT]
   step1 s = mapM number2post $ reverse $ sort $ number_from_post_name s

-}




dippersT_Handler :: Dippers -> Handler App App ()
dippersT_Handler p = renderWithSplices "dipper/dipper_base"
   ("entries" ##
   (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers) p
   )




link_is_local :: T.Text -> Bool
link_is_local l = T.isPrefixOf "/static/" l
   --where



splicesFrom_dippers :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers t = do
   mconcat $ [
     "dipper_url"        ## I.textSplice $ url t
    ,"dipper_url_img"    ## dipper_url_img_case
    ,"page_url"          ## I.textSplice $ page_url t
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




dipperT_individual_page_Handler :: (Dipper,[PostT]) -> Handler App App ()
dipperT_individual_page_Handler (d,sl) = renderWithSplices "dipper/dipper_individual_page_base"
   $ mconcat [

    splicesFrom_dippers d

   ,("references" ##
     (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) sl
    )

   ]











