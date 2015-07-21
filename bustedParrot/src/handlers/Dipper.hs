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
 ,dippersT_HandlerM
 ,give_dippers_references
 ,give_dippers_tags
 ,dipperT_individual_page_Handler
 ,dipperT_individual_page_HandlerM
 ,tag_c_groups_from_request_string
 ,tags_from_c_group
 ,operator_from_tag_name
 ,dippers_from_request_string
 ,give_all_used_tags
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
import Control.Monad.State
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page_common
import Nodes
import qualified Site_state as S
import Dipper_common
import InsertLinks









data Dipper_json = Dipper_json {
  miniature_json :: T.Text
 ,name_json :: T.Text
 ,url_json :: T.Text
 ,comment_json :: T.Text
} deriving (Show,Generic)


data Tag_operator = Or|And|Not|None


type Dippers_json = [Dipper_json]



instance FromJSON Dipper_json
instance ToJSON Dipper_json





dippersT_io :: IO Dippers
dippersT_io = do
   l<- getDirectoryContents "dippers"
   d <- mapM dipper_from_name_suffix $ number_from_dipper_name $ filter isDipperFile l
   mapM dipper_check_orientation $ concat d







dippersT_for_tag_io :: IO [(String, Dippers)]
dippersT_for_tag_io = do
   l<- getDirectoryContents "tags"
   let only_files = filter (\s-> not $ ((head s) == '.') || ((take 2 s) == ".." )) l
   d <- mapM (\s-> dipper_from_name $ "tags/" ++ s) $ only_files

   --d1<- mapM (mapM dipper_check_orientation) d
   {-putStrLn "------------------------------------------"
   putStrLn $ unlines $ filter (\s-> not $ ((head s) == '.') || ((take 2 s) == ".." )) l
   putStrLn $ show d
   putStrLn "------------------------------------------"
   -}
   return $ zip only_files d

   --dipper_from_name $ Fp.combine "tags" l













isDipperFile :: FilePath -> Bool
isDipperFile ('d':'i':'p':'p':'e':'r':'_':_) = True
isDipperFile _ = False



dipper_from_name_suffix :: String -> IO Dippers
dipper_from_name_suffix s = do
   d <- eitherDecode <$> B.readFile ("dippers/dipper_" ++ s ++ ".json"):: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map give_dipper dl



dipper_from_name :: String -> IO Dippers
dipper_from_name s = do
   d <- eitherDecode <$> B.readFile s :: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map give_dipper dl



number_from_dipper_name :: [FilePath] -> [String]
number_from_dipper_name fp = map (n . (drop 7)) fp
       where
         n x = (take ((length x)-5) ) x

{-
-}




give_dipper :: Dipper_json -> Maybe Dipper
give_dipper     (Dipper_json {miniature_json = _
                             ,name_json      = _
                             ,url_json       = ""
                             ,comment_json   = _
                             }) = Nothing

give_dipper     (Dipper_json {miniature_json = m
                             ,name_json      = n
                             ,url_json       = u
                             ,comment_json   = c
                             }) =
   Just $ Dipper {
     miniature = maybe_miniature
    ,name      = maybe_name
    ,page_url  = dipper_page_node_link' u
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




dipper_has_tag :: Dipper -> Dipper -> Bool
dipper_has_tag (Dipper {miniature = mt  -- tag
                       ,name      = nt
                       ,url       = ut
                       ,comment   = ct
                       })
               (Dipper {miniature = m   -- dipper
                       ,name      = n
                       ,url       = u
                       ,comment   = c
                       })
   |name_match = True
   |miniature_match && url_match = True
   |otherwise = False
   where
   miniature_match
     -- |mt == Nothing = True
      |m  == mt      = True
      |otherwise     = False

   name_match
      |nt == Nothing = False
      |n == nt   = True
      |otherwise = False

   url_match
      |u == ut   = True
      |ut == "*" = True
      |otherwise = False

   comment_match
    --  |ct == Nothing = True
      |c  == ct      = True
      |otherwise     = False




dipper_is_found_in_tags :: [(String, Dippers)] -> Dipper -> [String]
dipper_is_found_in_tags tags dipper  =
   (\(l,_) -> l) $ unzip $ filter (\(_,t) ->  any dipper_has_tag' t) tags
   where
   dipper_has_tag' = \tt-> dipper_has_tag tt dipper





give_dippers_tags :: IO [(Dipper,[String])]
give_dippers_tags = do
   d   <- dippersT_io
   ps  <- dippersT_for_tag_io
   return $ zip d $ map (dipper_is_found_in_tags ps) d



give_all_used_tags ::[(Dipper,[String])] -> [String]
give_all_used_tags dippers_with_tags = foldl' union [] $ snd $ unzip dippers_with_tags



give_dippers_of_a_tag :: String -> [(Dipper,[String])] -> Dippers
give_dippers_of_a_tag tag dippers_with_tags =
   (\(l,_) -> l) $ unzip $ filter (\(_,t)-> elem tag t) dippers_with_tags



dippers_from_request_string :: String -> [(Dipper,[String])] -> Dippers
dippers_from_request_string r dt =
   dippers_from_d_group $
     map (dippers_from_c_group' . operator_and_tag . tags_from_c_group) $
     tag_c_groups_from_request_string r
   where
   operator_and_tag :: [String] -> [(Tag_operator, String)]
   operator_and_tag t = zip (map operator_from_tag_name t) t

   dippers_from_c_group' :: [(Tag_operator, String)] -> Dippers
   dippers_from_c_group' ot = dippers_from_c_group ot dt


{-                                                                                                -}


dippers_from_d_group :: [Dippers] -> Dippers
dippers_from_d_group c = foldl' union [] c



dippers_from_c_group :: [(Tag_operator, String)] -> [(Dipper,[String])] -> Dippers
dippers_from_c_group c dt = foldl' op_and
                                   (give_dippers_of_a_tag (snd $ head c) dt)
                                   c
   where
   op_and :: Dippers -> (Tag_operator, String) -> Dippers
   op_and p (Not, t) = (\\)      p $ give_dippers_of_a_tag t dt
   op_and p (_  , t) = intersect p $ give_dippers_of_a_tag t dt



operator_from_tag_name :: String -> Tag_operator
operator_from_tag_name s = s2op $ head $ words $ map dash2space s
   where
   dash2space :: Char -> Char
   dash2space '-' = ' '
   dash2space  x  =  x

   s2op :: String -> Tag_operator
   s2op "not" = Not
   s2op "and" = And
   s2op "or"  = Or
   s2op  _    = None




tags_from_c_group :: String -> [String]
tags_from_c_group s = words $ map dot2space s
   where
   dot2space :: Char -> Char
   dot2space '.' = ' '
   dot2space  x  =  x




--conjunctive groups from disjunctive normal form
tag_c_groups_from_request_string :: String -> [String]
tag_c_groups_from_request_string s = words $ map understroke2space s
   where
   understroke2space :: Char -> Char
   understroke2space '_' = ' '
   understroke2space  x  =  x




dippersT_HandlerM :: Dippers -> [String] -> Int -> [String] -> State S.Routes (Handler App App ())
dippersT_HandlerM p tags page_number links = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper/dipper_base"
       $ mconcat $ [
       ("tags" ##
       (I.mapSplices $ I.runChildrenWith . splices_from_tag) tags
       )
      ,("entries" ##
       (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers) p
       )
      ,("pages" ##
       (I.mapSplices $ I.runChildrenWith . splices_from_page_number page_number)
          $ zip [1..length links] (links)
       )
      ,insertLinks $ Just nm
       ]





dippersT_Handler :: Dippers -> [String] -> Int -> [String] -> Handler App App ()
dippersT_Handler p tags page_number links = renderWithSplices "dipper/dipper_base"
   $ mconcat $ [
   ("tags" ##
   (I.mapSplices $ I.runChildrenWith . splices_from_tag) tags
   )
  ,("entries" ##
   (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers) p
   )
  ,("pages" ##
   (I.mapSplices $ I.runChildrenWith . splices_from_page_number page_number)
      $ zip [1..length links] (links)
   )
   ]








splicesFrom_dippers :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers t = do
   mconcat $ [
     "dipper_url"        ## I.textSplice $ url t
    ,"dipper_url_img"    ## dipper_url_img_case
    ,"page_url"          ## I.textSplice $ individual_dipper_node_link' $ T.unpack $ page_url t
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




splices_from_tag :: Monad n => String -> Splices (I.Splice n)
splices_from_tag tag = do
   mconcat $ [
     "tag_url"        ## I.textSplice $ tagged_node_link' tag 1
    ,"tag_style"        ## I.textSplice $ T.pack $ "tag"
    ,"tag"            ## I.textSplice $ T.pack $ tag
    ]


splices_from_page_number :: Monad n => Int -> (Int, String) -> Splices (I.Splice n)
splices_from_page_number number (page, link)
   |page == number= responce_current_page
   |otherwise = responce_other_page
   where
   responce_current_page = do
       mconcat $ [
         "page_url"        ## I.textSplice $ T.pack $ link
        ,"page_style"        ## I.textSplice $ T.pack $ "page_current"
        ,"page"            ## I.textSplice $ T.pack $ show $ page
        ]
   responce_other_page = do
       mconcat $ [
         "page_url"        ## I.textSplice $ T.pack $ link
        ,"page_style"        ## I.textSplice $ T.pack $ "page"
        ,"page"            ## I.textSplice $ T.pack $ show $ page
        ]





dipperT_individual_page_HandlerM :: (Dipper,[PostT]) -> State S.Routes (Handler App App ())
dipperT_individual_page_HandlerM (d,sl) = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper/dipper_individual_page_base"
       $ mconcat [

        splicesFrom_dippers d

       ,("references" ##
         (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) sl
        )
       ,insertLinks $ Just nm
       ]






dipperT_individual_page_Handler :: (Dipper,[PostT]) -> Handler App App ()
dipperT_individual_page_Handler (d,sl) = renderWithSplices "dipper/dipper_individual_page_base"
   $ mconcat [

    splicesFrom_dippers d

   ,("references" ##
     (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) sl
    )

   ]











