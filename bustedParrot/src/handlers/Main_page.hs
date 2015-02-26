{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

module Main_page
  (
    main_pageT_Handler
   ,splicesFrom_main_postsT_h
   ,postsT_h_io
   ,Routes(..)
   ,PostT(..)
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import Data.List
import Data.Monoid
import Control.Monad
import qualified Page as Pa
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

data PostT = PostT {
 postT :: Template
,number :: Int
}


data Routes = Routes {
 postsT :: [PostT]
,pagesT :: [Pa.PageT]
}









postsT_h_io :: IO [PostT]
postsT_h_io = do
   l<- getDirectoryContents "posts"
   mapM s2p $ reverse $ sort $ ff $ filter f l
   where
     f :: FilePath -> Bool
     f ('p':'o':'s':'t':_) = True
     f _ = False

     s2p :: Int -> IO PostT
     s2p s = do
       (Right s'@(DocumentFile {dfDoc=(TT.HtmlDocument {TT.docContent=docContent})})) <-
                                                        getDoc $ "posts/post" ++ show s ++ ".html"
       return $ PostT {postT=docContent, number=s}

     ff :: [FilePath] -> [Int]
     ff fp = map (read . n . (drop 4)) fp
       where
         n x = (take ((length x)-5) ) x






main_pageT_Handler :: [PostT] -> Handler App App ()
main_pageT_Handler p = renderWithSplices "main_page/main_posts"
   (mconcat
   [(
   (splicesFrom_main_postsT_h) $ head p
   ),
   ("posts_h" ##
   (I.mapSplices $ I.runChildrenWith . splicesFrom_main_postsT_h) $ take 9 $ tail p
   )]
   )




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
    ,"main_post_h_view_full_link"  ## I.textSplice $ T.pack $ "post" ++ show (number t) ++ ".html"    ]








