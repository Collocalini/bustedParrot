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
 ,dippersT_HandlerM
 ,dippersT_tagged_HandlerM
 ,give_dippers_references
 ,give_dippers_references'
 ,give_dippers_tags
 ,dipperT_individual_page_HandlerM
 ,tag_c_groups_from_request_string
 ,tags_from_c_group
 ,operator_from_tag_name
 ,dippers_from_request_string
 ,give_all_used_tags
 ,dipper_by_page_url
 ,dipperT_individual_page_HandlerM_tagged
) where

import Data.Aeson
import GHC.Generics
import Control.DeepSeq
import qualified Data.Maybe as M
import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory

import Data.List
import qualified Data.Map as Dm
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

import Control.Monad.State
import qualified Text.ParserCombinators.Parsec as P
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page_common
import Nodes
import qualified Site_state as S
import Dipper_common
import Dipper_image
import Dipper_entry_splices
import InsertLinks









data Dipper_json = Dipper_json {
  miniature_json :: T.Text
 ,name_json :: T.Text
 ,url_json :: T.Text
 ,altUrls_json :: Maybe [T.Text]
 ,comment_json :: T.Text
} deriving (Show,Generic)


data Tag_operator = Or|And|Not|None


type Dippers_json = [Dipper_json]



instance FromJSON Dipper_json
instance ToJSON Dipper_json





dippersT_io ::  Node_map -> IO Dippers
dippersT_io nm = do
  l<- getDirectoryContents "dippers"
  d <- (return.concat) 
        =<< (mapM (\x->dipper_from_name_suffix x nm)
              (number_from_dipper_name 
                $ filter isDipperFile 
                         $ reverse 
                         $ sort l
              )
            )
   
  cacheExists <- doesFileExist cacheFileName
  case cacheExists of
    False -> doSecondPassFullUpdate d
    True -> do
      j <- (return . decode) 
            =<< (B8.readFile cacheFileName)
      
      case j of
        Nothing -> doSecondPassFullUpdate d
        Just cached_d -> do
          d' <- doSecondPassWithCache cached_d d

          B8.writeFile 
            cacheFileName 
            $ encode d'
            
          return d'
       
  where
  cacheFileName = "dippers/meta.cache"
  
  doSecondPass d = mapM dipper_secondary_pass d
  
  doSecondPassFullUpdate d = do
    d' <- doSecondPass d
    B8.writeFile cacheFileName $ encode d'
    return d'
  
  doSecondPassWithCache :: [Dipper] -> [Dipper] -> IO [Dipper]
  doSecondPassWithCache 
    cached_d 
    d = mapM
          (\x -> M.maybe 
                  (dipper_secondary_pass x)
                  (return)
                  $ find (sameDipper x) cached_d
          )
          d
  
  sameDipper
    l@(Dipper 
        {url       = urlL
        ,altUrls   = altUrlsL
        }
      )
    r@(Dipper
        {url       = urlR
        ,altUrls   = altUrlsR
        }
      ) = (urlL == urlR) && (altUrlsL == altUrlsR)





dippersT_for_tag_io ::  Node_map -> IO [(String, Dippers)]
dippersT_for_tag_io nm = do
   l<- getDirectoryContents "tags"
   let only_files = filter (\s-> not $ ((head s) == '.') || ((take 2 s) == ".." )) l
   d <- mapM (\s-> tag_from_name  ("tags/" ++ s) nm) $ only_files
   return $ zip only_files d














isDipperFile :: FilePath -> Bool
isDipperFile ('d':'i':'p':'p':'e':'r':'_':_) = True
isDipperFile _ = False



dipper_from_name_suffix :: String -> Node_map -> IO Dippers
dipper_from_name_suffix s nm = do
   d <- eitherDecode <$> B.readFile ("dippers/dipper_" ++ s ++ ".json"):: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map (\x->give_dipper x nm) dl



dipper_from_name :: String -> Node_map -> IO Dippers
dipper_from_name s nm = do
   d <- eitherDecode <$> B.readFile s :: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map (\x->give_dipper x nm) dl




tag_from_name :: String -> Node_map -> IO Dippers
tag_from_name s nm = do
   d <- eitherDecode <$> B.readFile s :: IO (Either String Dippers_json)
   case d of
      Left err -> do putStrLn err
                     return []
      Right dl -> return $ M.catMaybes $ map (\x->give_tag x nm) dl





number_from_dipper_name :: [FilePath] -> [String]
number_from_dipper_name fp = map (n . (drop 7)) fp
       where
         n x = (take ((length x)-5) ) x

{-
-}



dipperIsSane :: Dipper_json -> Bool
dipperIsSane dj 
   --1
   |     ((url_json dj)       /= "")
      && ((miniature_json dj) /= "") = True
      
   --2
   |     altUrlsAreValid
      && ((miniature_json dj) /= "") = True
      
   |otherwise = False
   where
   altUrlsAreValid = 
         ((altUrls_json dj)   /= Nothing)  
            --managed to read json 
         
      && (not $ null $ filter (not.T.null) $ M.fromMaybe [] $ altUrls_json dj)
            --and input was not just some ["","",...]



give_dipper :: Dipper_json -> Node_map -> Maybe Dipper
give_dipper dj nm
   |dipperIsSane dj = give_dipper_common dj nm
   |otherwise       = Nothing



give_tag (Dipper_json {url_json = ""}) _ = Nothing
give_tag                           dj nm = give_dipper_common dj nm



give_dipper_common :: Dipper_json -> Node_map -> Maybe Dipper
give_dipper_common 
   (Dipper_json {miniature_json = m
                ,name_json      = n
                ,url_json       = u
                ,altUrls_json   = a
                ,comment_json   = c
                })
                 nm =
   Just $ Dipper {
     miniature   = maybe_miniature
    ,name        = maybe_name
    ,page_url    = dipper_page_node_link' $ url_substitution u nm
    ,url         = whatIsMainUrl
    ,altUrls     = maybe_altUrls
    ,url_raw     = whatIsMainUrl_raw
    ,altUrls_raw = maybe_altUrls_raw
    ,comment     = maybe_comment
    ,isVertical  = False
    ,scale       = DsNotDefined
    ,dipperType  = deduceDipperType $ T.unpack u   
    ,miniatureType = deduceDipperType $ T.unpack m 
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
     
   maybe_altUrls = map (\au->url_substitution au nm) $ M.fromMaybe [] a
   maybe_altUrls_raw = M.fromMaybe [] a
   
   
   whatIsMainUrl
     |u /= "" = url_substitution u nm
     |otherwise = head maybe_altUrls
   
   whatIsMainUrl_raw
     |u /= "" = u
     |otherwise = head maybe_altUrls_raw
   








dipper_secondary_pass :: Dipper -> IO Dipper
dipper_secondary_pass d
  |link_is_local $ url d = (loadImage $ T.unpack $ url d) >>= sp
  |otherwise = return $!! d

  where
    sp Nothing = return
      $!!
      (\de ->
        de {isVertical=False
           ,scale     = DsNotDefined
           -- ,dipperType = DtNotDefined
           -- ,miniatureType = DtNotDefined
           }) d


      
    sp (Just img) = return
      $!!
      (\de ->
        de {isVertical= image_is_vertical img
           ,scale     = deduceRepresentationScale img
           ,dipperType = deduceDipperType $ T.unpack $ url_raw d
           ,miniatureType = mbDipperType $ miniature d}) d
       

    mbDipperType (Just m) = deduceDipperType $ T.unpack m
    mbDipperType _        = DtNotDefined








url_substitution :: T.Text -> Node_map -> T.Text
url_substitution url nm =
   case (P.parse (url_substitution_parse_nodes) "(unknown)" $ T.unpack url) of
      Left e -> url
      Right r -> url_substitution_substitute_nodes r nm




url_substitution_parse_nodes :: P.Parser ([Either T.Text T.Text])
url_substitution_parse_nodes = do
  s1<- P.optionMaybe $ P.manyTill P.anyChar (P.string "${")
  case s1 of
     Just s -> do
                  s2<- P.manyTill P.anyChar (P.string "}")
                  next <- url_substitution_parse_nodes
                  return $ [(Left $ T.pack s),(Right $ T.pack s2)] ++ next
     Nothing -> do
                  s2<- P.many P.anyChar
                  return $ [Left $ T.pack s2]







url_substitution_substitute_nodes :: [Either T.Text T.Text] -> Node_map -> T.Text
url_substitution_substitute_nodes s nm = T.concat $ map step1 s
   where
   step1 :: Either T.Text T.Text -> T.Text
   step1 (Left n) = n
   step1 (Right n) = node_to_link n nm








give_all_posts :: IO [(String, T.Text)]
give_all_posts = do
   l<- getDirectoryContents "posts"
   let l' = filter isPostFile l
   let files = map snd $ sortBy give_all_posts_sortingFunction $ zip (number_from_post_name l') 
                                                                      l'
   r<- mapM (\n-> readFile ("posts/" ++ n)) files
   return $ zip files $ map T.pack r


give_all_posts_sortingFunction (n1,_) (n2,_) = compare n1 n2


give_all_posts' :: IO [(String, T.Text)]
give_all_posts' = do
   l<- getDirectoryContents "posts"
   let l' = filter isPostFile l
   let files = map snd $ sortBy give_all_posts_sortingFunction $ zip (number_from_post_name l') 
                                                                      l'
   r<- mapM (\n-> readFile ("posts/" ++ n)) files
   return $!! zip files $ map T.pack r



dipper_is_found_in :: [(String, T.Text)] -> Dipper -> [String]
dipper_is_found_in posts
   (Dipper {
     miniature = _
    ,name      = _
    ,page_url  = _
    ,url       = u
    ,url_raw   = u_raw
    ,comment   = _
    })  = (\(l,_) -> l) $ unzip $! filter (\(_,t) -> (T.isInfixOf u t)||(T.isInfixOf u_raw t)) posts




dipper_by_page_url :: T.Text -> [(Dipper, a)] -> Maybe (Dipper, a)
dipper_by_page_url s ds = find (\d -> (cmp d) == s) ds
   where
   cmp d = (\((Dipper {page_url  = p}), _) -> p) d



dippers_references :: Dippers -> [(String, T.Text)] -> [[String]]
dippers_references d posts = map (dipper_is_found_in posts) d


dippers_references' :: Dippers -> [(String, T.Text)] -> [[String]]
dippers_references' d posts = force $ map (dipper_is_found_in posts) d



give_dippers_references :: Node_map -> IO [(Dipper,[PostT])]
give_dippers_references nm = do
   d   <- dippersT_io nm
   ps  <- give_all_posts
   pTs <- mapM step1 $!! dippers_references d ps
   return $ zip d pTs
   where
   step1 :: [String] -> IO [PostT]
   step1 s = mapM number2post $ reverse $ sort $ number_from_post_name s



give_dippers_references' :: Node_map -> Dippers -> IO [(Dipper,[PostT])]
give_dippers_references' nm d = do
   ps  <- give_all_posts'
   pTs <- mapM step1 $!! dippers_references d ps
   return $ zip d $! pTs
   where
   step1 :: [String] -> IO [PostT]
   step1 s = mapM number2post $! reverse $ sort $ number_from_post_name s

   step2 x = do
     n2p@(PostT {postT=p, number=n})<-number2post x
     print p
     return n2p
     


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





give_dippers_tags :: Node_map -> IO [(Dipper,[String])]
give_dippers_tags nm = do
   d   <- dippersT_io nm
   ps  <- dippersT_for_tag_io nm
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




dippers_neighbors :: Dipper -> Dippers -> (Maybe Dipper, Maybe Dipper)
dippers_neighbors d ds = step1 dippers_index
   where
   dippers_index = elemIndex d ds
   prev_dipper i
      |i==0 = Nothing
      |otherwise = Just $ ds !! (i-1)

   next_dipper i
      |i < (length ds)-1 = Just $ ds !! (i+1)
      |otherwise = Nothing

   step1 (Just i) = (prev_dipper i, next_dipper i)
   step1 (Nothing) = (Nothing, Nothing)






--dippersT_HandlerM_common :: Dippers -> [String] -> Int -> [String] -> Node_map -> a
dippersT_HandlerM_common tags page_number links nm = do
   mconcat $ [
       ("tags" ##
       (I.mapSplices $ I.runChildrenWith . splices_from_tag) tags
       )

      ,("pages" ##
       (I.mapSplices $ I.runChildrenWith . splices_from_page_number page_number)
          $ zip [1..length links] (links)
       )
      ,insertLinks $ Just nm
       ]


dippersT_HandlerM :: Dippers -> [String] -> Int -> [String] -> State S.Routes (Handler App App ())
dippersT_HandlerM p tags page_number links = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper_base"
       $ mconcat $ [
       dippersT_HandlerM_common tags page_number links nm
      ,("entries" ##
       -- (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers) p
       (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers_entry_case) p
       )
       ]



dippersT_tagged_HandlerM :: Dippers ->
                           [String] ->
                             String -> Int -> [String] -> State S.Routes (Handler App App ())
dippersT_tagged_HandlerM p tags tag page_number links = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper_base"
       $ mconcat $ [
       dippersT_HandlerM_common tags page_number links nm
      ,("entries" ##
       (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers_entry_case_tags tag) p
       )
       ]











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





dipperT_individual_page_navigation :: Monad n => (Maybe Dipper, Maybe Dipper) -> Splices (I.Splice n)
dipperT_individual_page_navigation (p,n) = do
   mconcat $ [
     "nav_prev"   ## dipperT_individual_page_nav_no_nav p $ dipperT_individual_page_nav_prev p
    ,"nav_next"   ## dipperT_individual_page_nav_no_nav n $ dipperT_individual_page_nav_next n
    ]



dipperT_individual_page_navigation_tagged :: Monad n => (Maybe Dipper, Maybe Dipper) -> String -> Splices (I.Splice n)
dipperT_individual_page_navigation_tagged (p,n) tags = do
   mconcat $ [
     "nav_prev"   ## dipperT_individual_page_nav_no_nav p $ dipperT_individual_page_nav_prev_tagged p tags
    ,"nav_next"   ## dipperT_individual_page_nav_no_nav n $ dipperT_individual_page_nav_next_tagged n tags
    ]




dipperT_individual_page_nav_common :: Monad n => Maybe Dipper -> Splices (I.Splice n)
dipperT_individual_page_nav_common d = do
   mconcat $ [
     "url"        ## I.textSplice $ nav_url d
    ,"style"      ## I.textSplice $ nav_style d
    ]
   where
   nav_url :: Maybe Dipper -> T.Text
   nav_url (Just ( Dipper {page_url=pu})) = individual_dipper_node_link' $ T.unpack $ pu
   nav_url Nothing = ""

   nav_style (Just _ ) = "dipper_navig"
   nav_style Nothing   = "dipper_no_navig"



dipperT_individual_page_nav_common_tagged :: Monad n => Maybe Dipper -> String -> Splices (I.Splice n)
dipperT_individual_page_nav_common_tagged d tag = do
   mconcat $ [
     "url"        ## I.textSplice $ nav_url d
    ,"style"      ## I.textSplice $ nav_style d
    ]
   where
   nav_url :: Maybe Dipper -> T.Text
   nav_url (Just t@( Dipper {page_url=pu})) = individual_dipper_tagged_page_link' t tag
   nav_url Nothing = ""

   nav_style (Just _ ) = "dipper_navig"
   nav_style Nothing   = "dipper_no_navig"



dipperT_individual_page_nav_prev :: Monad n => Maybe Dipper -> Splices (I.Splice n)
dipperT_individual_page_nav_prev d = do
   mconcat $ [
     dipperT_individual_page_nav_common d
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_prev" (return ())
    ]


dipperT_individual_page_nav_next :: Monad n => Maybe Dipper -> Splices (I.Splice n)
dipperT_individual_page_nav_next d = do
   mconcat $ [
     dipperT_individual_page_nav_common d
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_next" (return ())
    ]



dipperT_individual_page_nav_prev_tagged :: Monad n => Maybe Dipper -> String -> Splices (I.Splice n)
dipperT_individual_page_nav_prev_tagged d tags = do
   mconcat $ [
     dipperT_individual_page_nav_common_tagged d tags
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_prev" (return ())
    ]


dipperT_individual_page_nav_next_tagged :: Monad n => Maybe Dipper -> String -> Splices (I.Splice n)
dipperT_individual_page_nav_next_tagged d tags = do
   mconcat $ [
     dipperT_individual_page_nav_common_tagged d tags
    ,"nav"      ## I.callTemplate "dipper_individual_page_nav_next" (return ())
    ]


dipperT_individual_page_nav_no_nav (Just d) t = do
   I.callTemplate "dipper_individual_page_nav" t

dipperT_individual_page_nav_no_nav Nothing t = do
   I.callTemplate "dipper_individual_page_no_nav" t










--dipperT_individual_page_HandlerM_common :: (Dipper,[PostT]) -> Dippers -> State S.Routes (Handler App App ())
dipperT_individual_page_HandlerM_common (d,sl) selection nm tag = do
   mconcat [
        ("references" ##
         (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h_common tag) sl
        )
       ,insertLinks $ Just nm
       ]





dipperT_individual_page_HandlerM :: (Dipper,[PostT]) -> Dippers -> State S.Routes (Handler App App ())
dipperT_individual_page_HandlerM (d,sl) selection = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper_individual_page_base"
       $ mconcat [
        dipperT_individual_page_navigation $ dippers_neighbors d selection
       ,splicesFrom_individual_dipper_case d
       ,dipperT_individual_page_HandlerM_common (d,sl) selection nm (Just $ cmp d)
       ]
   where
   cmp d = T.unpack $ (\(Dipper {page_url  = p}) -> p) d
    

dipperT_individual_page_HandlerM_tagged :: (Dipper,[PostT]) -> Dippers -> String -> State S.Routes (Handler App App ())
dipperT_individual_page_HandlerM_tagged (d,sl) selection tags = do
   (S.Routes {S.node_map=nm}) <- get
   return $ renderWithSplices "dipper_individual_page_base"
       $ mconcat [
        dipperT_individual_page_navigation_tagged (dippers_neighbors d selection) tags
       ,splicesFrom_individual_dipper_case_tags tags d
       ,dipperT_individual_page_HandlerM_common (d,sl) selection nm (Just $ cmp d)
       ]
  
   where
   cmp d = T.unpack $ (\(Dipper {page_url  = p}) -> p) d









