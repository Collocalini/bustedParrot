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
{-# LANGUAGE OverloadedStrings #-}

module Main_page_common (
 PostT(..)
,postsT_h_io
,isPostFile
,number2post
,number_from_post_name
,splicesFrom_main_postsT_h
--,splicesFrom_main_postsT_h_M
) where



import           Heist
import qualified Text.XmlHtml as TT
import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import Nodes
--import InsertLinks

{-
import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import           Application
-}
--import Control.Monad
--import qualified Page as Pa
------------------------------------------------------------------------------



data PostT = PostT {
 postT :: Template
,number :: Int
}



postsT_h_io :: IO [PostT]
postsT_h_io = do
   l<- getDirectoryContents "posts"
   mapM number2post $ reverse $ sort $ number_from_post_name $ filter isPostFile l






isPostFile :: FilePath -> Bool
isPostFile ('p':'o':'s':'t':_) = True
isPostFile _ = False


number2post :: Int -> IO PostT
number2post s = do
   (Right (DocumentFile {dfDoc=(TT.HtmlDocument {TT.docContent=docContent})})) <-
                                                getDoc $ "posts/post" ++ show s ++ ".html"
   return $ PostT {postT=docContent, number=s}


number_from_post_name :: [FilePath] -> [Int]
number_from_post_name fp = map (read . n . (drop 4)) fp
   where
    n x = (take ((length x)-5) ) x




{-

splicesFrom_main_postsT_h_M :: Monad n => PostT -> Node_map -> Splices (I.Splice n)
splicesFrom_main_postsT_h_M t nm = do
   mconcat $ (["post_h"  ## return [head $ postT t]
      ,insertLinks $ Just nm
      ] ++ step1 (tail $ postT t) )
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
  {-step1 [_,_] =
    ["main_post_h_view_full_caption"  ## I.textSplice ""
    ,"main_post_h_view_full_style"  ## I.textSplice "display:none"
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ ""
    ]-}
  step1 _ =
    ["main_post_h_view_full_caption"  ## I.textSplice "Читать дальше / Read more"
    ,"main_post_h_view_full_style"  ## I.textSplice ""
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ "/post" ++ show (number t) ++ ".html"    ]


-}






splicesFrom_main_postsT_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_main_postsT_h t = do
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
  {-step1 [_,_] =
    ["main_post_h_view_full_caption"  ## I.textSplice ""
    ,"main_post_h_view_full_style"  ## I.textSplice "display:none"
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ ""
    ]-}
  step1 _ =
    ["main_post_h_view_full_caption"  ## I.textSplice "Читать дальше / Read more"
    ,"main_post_h_view_full_style"  ## I.textSplice ""
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ "/post" ++ show (number t) ++ ".html"    ]






