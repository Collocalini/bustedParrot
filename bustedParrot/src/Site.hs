{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
   ,Routes(..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Heist
import qualified Heist.Compiled as C
import           Data.Monoid
import           Control.Monad.State
import           Control.Applicative

import qualified Main_page as MP
import qualified Post as P
import qualified Page as Pa
import qualified Archive as A
import qualified Dipper as D
------------------------------------------------------------------------------


data Routes = Routes {
 postsT :: [MP.PostT]
,pagesT :: [Pa.PageT]
,dippersT :: [D.Dippers]
}



-- | The application's routes.
routes :: State Routes [(ByteString, Handler App App ())]
routes = do
    (Routes {postsT=postsT, pagesT=pagesT, dippersT=dippersT}) <- get
    return $ [ ("", MP.main_pageT_Handler postsT)
              ,("archive.html", A.archive_Handler postsT)
              ,("static", serveDirectory "static")
             ] ++ (generate_postN_response postsT)
               ++ (generate_pageWTWR_response pagesT)
               ++ (generate_dippers_pageN_response dippersT)


generate_postN_response :: [MP.PostT] -> [(ByteString, Handler App App ())]
generate_postN_response p = map (\x@(MP.PostT {MP.number=n}) ->
    (B.pack $ "post" ++ (show n) ++ ".html",
     P.post_Handler x))  p


generate_pageWTWR_response :: [Pa.PageT] -> [(ByteString, Handler App App ())]
generate_pageWTWR_response p = map (\x@(Pa.PageT {Pa.name=n}) ->
    (B.pack $ "page" ++ n ++ ".html",
     Pa.page_Handler x))  p

generate_dippers_pageN_response :: [D.Dippers] -> [(ByteString, Handler App App ())]
generate_dippers_pageN_response p = [(B.pack "page_my_pictures.html", D.dippersT_Handler $ head p)]


{-- ================================================================================================
================================================================================================ --}
{-
main_css_Handler :: Snap ()
main_css_Handler = do
    index <- liftIO $ readFile "./static/select_folder.html"
    writeBS $ B.fromString index
-----------------------------------------------------------------------------------------------------
-}

------------------------------------------------------------------------------
-- | Compose all the compiled splices imported from the handler modules
allCompiledSplices :: Monad n => Splices (C.Splice n)
allCompiledSplices = mconcat []

------------------------------------------------------------------------------
-- | The application initializer.
app :: State Routes (SnapletInit App App)
app = do
    r<- routes
    return (makeSnaplet "app" "A snap demo application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ mempty { hcCompiledSplices = allCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes r
    return $ App h s)

--------------------------------------------------------------------------------
-- | Our glorious index page
--indexHandler :: Handler App App ()
--indexHandler = render "index"
