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
import qualified Data.Text as T
import qualified Data.Map as DMap
import Data.List
import Data.Maybe
import qualified Data.Char as C
import           Snap.Core
--import           Snap.Http.Server
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
--import           Control.Applicative

import qualified Main_page as MP
import qualified Main_page_common as MPC
import qualified Post as P
import qualified Page as Pa
import qualified Archive as A
import qualified Dipper as D
import Site_state
------------------------------------------------------------------------------




-- | The application's routes.
routes :: State Routes [(ByteString, Handler App App ())]
routes = do
   (Routes { postsT=postsT
            ,pagesT=pagesT
            ,dippersT=dippersT
            ,dippers_references=dippers_references
            ,dippers_tags=dippers_tags}) <- get

   post_r<- generate_postN_responseM
   main_page_r<- MP.main_pageT_HandlerM postsT

   return $ concat
      [
        (routes_pure postsT pagesT dippersT dippers_references dippers_tags)
       ,post_r
       ,[(B.pack "", main_page_r )]
      ]

   where
   routes_pure postsT pagesT dippersT dippers_references dippers_tags =
      [ --("", MP.main_pageT_Handler postsT)
        ("/archive.html", A.archive_Handler postsT)
       ,("/static", serveDirectory "static") ]
       -- ++ (generate_postN_response postsT)
         ++ (generate_pageWTWR_response pagesT)
         ++ (generate_dippers_pageN_response dippersT dippers_tags)
         ++ (generate_dippers_individual_page_response dippers_references)
         ++ (generate_dippers_tags_response dippers_tags)





generate_postN_response :: [MPC.PostT] -> [(ByteString, Handler App App ())]
generate_postN_response p = map (\x@(MPC.PostT {MPC.number=n}) ->
    (B.pack $ "/post" ++ (show n) ++ ".html",
     P.post_Handler x))  p




generate_postN_responseM :: State Routes [(ByteString, Handler App App ())]
generate_postN_responseM = do
   (Routes { postsT=p}) <- get
   phl<- mapM (\x -> P.post_HandlerM x) p
   return $ zip (r p) phl
   where
   r p = map (\MPC.PostT {MPC.number=n} -> B.pack $ "/post" ++ (show n) ++ ".html") p




generate_pageWTWR_response :: [Pa.PageT] -> [(ByteString, Handler App App ())]
generate_pageWTWR_response p = map (\x@(Pa.PageT {Pa.name=n}) ->
    (B.pack $ "/pages/page" ++ n ++ ".html",
     Pa.page_Handler x))  p

generate_dippers_pageN_response :: D.Dippers -> [(D.Dipper,[String])] -> [(ByteString, Handler App App ())]
generate_dippers_pageN_response p dt = zip routes responces
   where
   links = map ((\x-> "/dippers/dippers_" ++ x ++ ".html").show) total_responces
   routes = map B.pack links
   responce i t
      |total_pages'>1= D.dippersT_Handler (give_page i p) (D.give_all_used_tags dt) i links
      |otherwise     = D.dippersT_Handler (give_page i p) (D.give_all_used_tags dt) i []
   responces = map (\x-> responce x total_pages') total_responces
   total_responces = [1.. total_pages']
   total_pages' = (total_pages p)




generate_dippers_individual_page_response :: [(D.Dipper,[MPC.PostT])] -> [(ByteString, Handler App App ())]
generate_dippers_individual_page_response p = map step1 p
   where
   step1 :: (D.Dipper,[MPC.PostT]) -> (ByteString, Handler App App ())
   step1 a@(d,_) =
      (
       B.pack $ ("/individual_dippers/" ++)  $ T.unpack $ D.page_url d
      ,D.dipperT_individual_page_Handler a
      )



generate_dippers_tags_response :: [(D.Dipper,[String])] -> [(ByteString, Handler App App ())]
generate_dippers_tags_response p = [(route , handler)]
   where

   route = B.pack "/tagged/:tag"

   handler = do
     tags <- getParams
     D.dippersT_Handler (dippers_from_request_string' tags)
                        (D.give_all_used_tags p)
       (fromMaybe 1 $ request_page_number tags)
       (links tags (dippers_from_request_string'' tags))


   links tags d
      |(total_pages d) > 1 = map (\x-> "/tagged/"
                         ++ (request_with_no_page_number tags)
                         ++ (show x)
                         ++ ".html") [1..total_pages d]
      |otherwise = []

   dippers_on_page_n :: Int -> D.Dippers -> D.Dippers
   dippers_on_page_n i d
      |i<=1 = take max_items_per_page d
      |(fromIntegral i)<=total_pages d = give_page i d
      |otherwise = take max_items_per_page $ reverse d
      where


   dippers_from_request_string' tags =
           dippers_on_page_n (fromMaybe 1 $ request_page_number tags)
                             (dippers_from_request_string'' tags)





   dippers_from_request_string'' tags = D.dippers_from_request_string
           (
           request_with_no_page_number tags
           ) p



request tags = reverse $ drop 5 $ reverse $ B.unpack $ B.concat $
                                                                  DMap.findWithDefault [] "tag" tags

request_page_number tags = try_read $ reverse $ takeWhile C.isDigit $ reverse $ request tags
  where
  try_read :: String -> Maybe Int
  try_read [] = Nothing
  try_read x  = Just $ sane $ read x

  sane i
     |i<=1 = 1
     |otherwise = i


request_with_no_page_number tags = reverse $ dropWhile C.isDigit $ reverse $ request tags



total_items d = length $ items d
total_pages d = ceiling ((fromIntegral $ total_items d) / (fromIntegral max_items_per_page))
items d = zip (concat $ map (replicate max_items_per_page) [1..]) d
give_page i d = snd $ unzip $ head $ drop (i-1) $ groupBy (\(n,_) (n1,_)-> n==n1) $ items d



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
