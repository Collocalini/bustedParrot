-----------------------------------------------------------------------------
--
-- Module      :  Main_page_common
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
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main_page_common (
 PostT(..)
,postsT_h_io
,isPostFile
,number2post
,number_from_post_name
,splicesFrom_main_postsT_h
,splicesFrom_main_postsT_h_common
,postsT_to_neigbours
,postT_set_neigbours
,postsT_derive_neigbours
) where



import           Heist
import qualified Text.XmlHtml as TT
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import Nodes
--import GHC.Generics (Generic)
import Control.DeepSeq

data PostT = PostT {
 postT :: Template
,number :: Int
,next   :: Maybe Int
,prev   :: Maybe Int
} --deriving (Generic, NFData)



postsT_h_io :: IO [PostT]
postsT_h_io = do
   l<- getDirectoryContents "posts"
   mapM number2post3 $!! postsT_names_to_neigbours l



postsT_names_to_neigbours :: [String] -> [(Int,Maybe Int, Maybe Int)]
postsT_names_to_neigbours l = 
   zip3 (reverse lnumber)
        (reverse lprev')
        (reverse lnext')

  where
  lnumber = sort $ number_from_post_name $ filter isPostFile l
  lprev' = lprev lnumber
  lnext' = lnext lnumber
  
lprev lnumber = Nothing:(map (\x-> Just x) $ take ((length lnumber) -1) lnumber)
lnext lnumber = concat [(map (\x-> Just x) $ drop 1 lnumber), [Nothing]]


postsT_to_neigbours :: Bool -> [PostT] -> [(Int,Maybe Int, Maybe Int)]
postsT_to_neigbours doReverse l = zip3 (doReverseIfRequired lnumber)
                                       (doReverseIfRequired lprev')
                                       (doReverseIfRequired lnext')
  where
  number (PostT {postT=_, number=n, next=_, prev=_}) = n
  lnumber = map number l
  lprev' = lprev lnumber
  lnext' = lnext lnumber
  
  doReverseIfRequired = rev doReverse
  
  
  rev True = reverse
  rev False = (\x->x)


postT_set_neigbours :: (PostT,Maybe Int, Maybe Int) -> PostT
postT_set_neigbours (p,prev,next) = result p
   where
   result :: PostT -> PostT
   result (PostT {postT=docContent, number=s, next=_, prev=_}) = 
      PostT {postT=docContent, number=s, next=next, prev=prev}


postsT_derive_neigbours :: Bool -> [PostT] -> [PostT]
postsT_derive_neigbours doReverse ps = 
   map postT_set_neigbours
      $ map (\(p,(_,l,r))->(p,l,r))
      $ zip ps 
            $ postsT_to_neigbours doReverse ps
   


isPostFile :: FilePath -> Bool
isPostFile ('p':'o':'s':'t':_) = True
isPostFile _ = False


number2post :: Int -> IO PostT
number2post s = do
   putStrLn $ "number2post " ++ show s
   (Right (DocumentFile {dfDoc=(TT.HtmlDocument {TT.docContent=docContent})})) <-
                                                getDoc $ "posts/post" ++ show s ++ ".html"
   return $! PostT {postT=docContent, number=s, next=Nothing, prev=Nothing}
   
   
number2post3 :: (Int,Maybe Int, Maybe Int) -> IO PostT
number2post3 (s,prev,next) = do
   postTpl <- number2post s
   return $ postT_set_neigbours (postTpl, prev, next)
   


number_from_post_name :: [FilePath] -> [Int]
number_from_post_name fp = map (read . n . (drop 4)) fp
   where
    n x = (take ((length x)-5) ) x








splicesFrom_main_postsT_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_main_postsT_h t = splicesFrom_main_postsT_h_common Nothing t

splicesFrom_main_postsT_h_tagged :: Monad n => String -> PostT -> Splices (I.Splice n)
splicesFrom_main_postsT_h_tagged tag t = splicesFrom_main_postsT_h_common (Just tag) t

splicesFrom_main_postsT_h_common :: Monad n => Maybe String -> PostT -> Splices (I.Splice n)
splicesFrom_main_postsT_h_common tag t = do
   mconcat $ (["post_h"  ## return [head $ postT t]] ++ step1 (tail $ postT t) )
  where
  step1 [] =
    ["main_post_h_view_full_caption"  ## I.textSplice ""
    ,"main_post_h_view_full_style"  ## I.textSplice "display:none"
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ "/"
    ]
  step1 [_] =
    ["main_post_h_view_full_caption"  ## I.textSplice ""
    ,"main_post_h_view_full_style"  ## I.textSplice "display:none"
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ "/"
    ]
  step1 _ =
    ["main_post_h_view_full_caption"  ## I.textSplice "Читать дальше / Read more"
    ,"main_post_h_view_full_style"  ## I.textSplice ""
    ,"main_post_h_view_full_link"  ## I.textSplice $ postNodeLink (number t) tag ]

  
  postNodeLink i Nothing = post_node_link' i
  postNodeLink i (Just ttt) = post_node_link_tagged'' i ttt



