{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
   ,Routes(..)
   ,total_pages_archive
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Map as DMap
import Data.List
import Data.Maybe
--import Control.DeepSeq
import qualified Data.Char as C
import           Snap.Core
--import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Interpreted
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Heist
import qualified Heist.Interpreted as I
import           Data.Monoid
import           Control.Monad.State
import           Control.Applicative
import           Control.Lens

import qualified Main_page as MP
import qualified Main_page_common as MPC
import qualified Post as P
import qualified Page as Pa
import qualified Page_common as PaC
import qualified Archive as A
import qualified Dipper as D
import Site_state
import Nodes
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
   --archive_r<-A.archive_HandlerM postsT
   archive_r_e<-generate_archive_pageN_responseM_earliest
   archive_r_l<-generate_archive_pageN_responseM_latest
   pagewtwr_r<- generate_pageWTWR_responseM
   dippers_pn_r<- generate_dippers_pageN_responseM
   dippers_ip_r<- generate_dippers_individual_page_responseM
   dippers_ti_r<- generate_dippers_tagged_individual_responseM
   dippers_tags<- generate_dippers_tags_responseM

   return $ concat
      [
        (routes_pure postsT pagesT dippersT dippers_references dippers_tags)
       ,[ ("", main_page_r )
         --,("/archive.html", archive_r)
         ]
       ,post_r
       ,archive_r_e
       ,archive_r_l
       ,pagewtwr_r
       ,dippers_pn_r
       ,dippers_ip_r
       ,dippers_ti_r
       ,dippers_tags
      ]

   where
   routes_pure postsT pagesT dippersT dippers_references dippers_tags =
      [ --("", MP.main_pageT_Handler postsT)
       -- ("/archive.html", A.archive_Handler postsT)
        ("/static", serveDirectory "static")
       ]
       -- ++ (generate_postN_response postsT)
       --  ++ (generate_pageWTWR_response pagesT)
       --  ++ (generate_dippers_pageN_response dippersT dippers_tags)
       --  ++ (generate_dippers_individual_page_response dippers_references)
       --  ++ (generate_dippers_tags_response dippers_tags)




generate_postN_responseM :: State Routes [(ByteString, Handler App App ())]
generate_postN_responseM = do
   (Routes { postsT=p}) <- get
   phl<- mapM (\x -> P.post_HandlerM x) p
   return $ zip (r p) phl
   where
   r p = map (\MPC.PostT {MPC.number=n} -> post_node_link n) p





generate_pageWTWR_responseM :: State Routes [(ByteString, Handler App App ())]
generate_pageWTWR_responseM = do
   (Routes { pagesT=p}) <- get
   phl<- mapM (\x -> Pa.page_HandlerM x) p
   return $ zip (r p) phl
   where
   r p = map (\PaC.PageT {PaC.name=n} -> page_node_link n) p






generate_archive_pageN_responseM_earliest :: State Routes [(ByteString, Handler App App ())]
generate_archive_pageN_responseM_earliest = do
   generate_archive_pageN_responseM_common A.EarliestFirst


generate_archive_pageN_responseM_latest :: State Routes [(ByteString, Handler App App ())]
generate_archive_pageN_responseM_latest = do
   generate_archive_pageN_responseM_common A.LatestFirst




generate_archive_pageN_responseM_common
  :: A.ArchivePageOrder -- page order (earliest first/latest first)
  -> State Routes [(ByteString, Handler App App ())]
generate_archive_pageN_responseM_common po = do
   r@(Routes { postsT=postsT, number_of_pages_in_archive=nopia}) <- get
   --ar<- responces postsT r
   --return $ zip (routes postsT) ar
   case po of
     A.EarliestFirst -> return $ zip (routes_e postsT) $ responces postsT r nopia po
     A.LatestFirst   -> return $ zip (routes_l postsT) $ responces postsT r nopia po

   where
   links_e p = map archive_pageN_node_link'' (total_responces p)
   routes_e p = map B.pack $ links_e p

   links_l p = map archive_latest_first_pageN_node_link'' (total_responces p)
   routes_l p = map B.pack $ links_l p

   responce i po t p routes gpa links
      |t >1 =
         evalState
           A.archive_HandlerM
           (A.Archive_Handler {
                A.posts       = (gpa i p)
               ,A.page_number = i
               ,A.links       = (links p)
               ,A.routes      = routes
               ,A.page_order  = po
               })
                  --(give_page i p) i $ links p

      |otherwise =
         evalState
           A.archive_HandlerM
           (A.Archive_Handler {
                A.posts       = (gpa i p)
               ,A.page_number = i
               ,A.links       = []
               ,A.routes      = routes
               ,A.page_order  = po
               })
                  --(give_page i p) ) i []

   responces p r t A.EarliestFirst =
     map (\x-> responce x A.EarliestFirst t p r give_page_archive links_e)
            $ total_responces p
   responces p r t A.LatestFirst   =
     map (\x-> responce x A.LatestFirst t p r give_page_archive_latest_first links_l)
            $ total_responces p


   total_responces p = [1.. total_pages' p]
   total_pages' p = (total_pages_archive p)


--data ArchivePageOrder = EarliestFirst|LatestFirst


generate_dippers_pageN_responseM :: State Routes [(ByteString, Handler App App ())]
generate_dippers_pageN_responseM = do
   (Routes { dippersT=dippersT
            ,dippers_tags=dippers_tags}) <- get
   dr<- responces dippersT dippers_tags
   return $ zip (routes dippersT) dr
   --return [("", D.dippersT_Handler (give_page 1 dippersT) (D.give_all_used_tags dippers_tags) 1 [] )]
   where
   links p = map dippers_pageN_node_link'' (total_responces p)
   routes p = map B.pack $ links p
   responce i t p dt
      |total_pages' p >1= D.dippersT_HandlerM (give_page i p) (D.give_all_used_tags dt) i $ links p
      |otherwise     = D.dippersT_HandlerM (give_page i p) (D.give_all_used_tags dt) i []
   --responces :: D.Dippers -> [(D.Dipper,[String])] -> State Routes [ Handler App App ()]
   responces p dt = mapM (\x-> responce x total_pages' p dt) $ total_responces p
   total_responces p = [1.. total_pages' p]
   total_pages' p = (total_pages p)




generate_dippers_individual_page_responseM :: State Routes [(ByteString, Handler App App ())]
generate_dippers_individual_page_responseM = do
   (Routes {dippers_references=dippers_references, dippersT=dippersT}) <- get
   (return.concat) =<< (mapM (step1 dippersT) dippers_references)
   where
   step1 ::  D.Dippers -> (D.Dipper,[MPC.PostT]) -> State Routes [(ByteString, Handler App App ())]
   step1 dT a@(d,ps) = do

      idp<- D.dipperT_individual_page_HandlerM a dT
      idpDR <- mapM step2 $ zip (repeat d) 
                                ps
      return ((
         individual_dipper_node_link  $ T.unpack $ D.page_url d
        ,idp
        ):idpDR
        )

   step2 :: (D.Dipper,MPC.PostT) -> State Routes (ByteString, Handler App App ())
   step2 (d,p) = do
      
      pht<- P.post_HandlerM_tagged p $ T.unpack $ D.page_url d
      
      return (
           post_node_link_tagged' (postNumber p) $ T.unpack $ D.page_url d
          ,pht
         )
   

   postNumber p = (\(MPC.PostT {MPC.number  = n}) -> n) p


generate_dippers_tagged_individual_responseM :: State Routes [(ByteString, Handler App App ())]
generate_dippers_tagged_individual_responseM = do
   s@(Routes {dippers_references=dippers_references, dippers_tags=dippers_tags}) <- get
   --return [(route , handler dippers_tags dippers_references s)]
   return $ zip (routes dippers_references) (map (\dr-> handler' dippers_tags dr s) dippers_references)
   --return []
   where


   route = individual_dipper_tagged_request_link

   routes dr = map individual_dipper_tagged_node_link $ fst $ unzip dr

   {-
   handler :: [(D.Dipper,[String])] -> [(D.Dipper,[MPC.PostT])] -> Routes -> Handler App App ()
   handler dt dr s = do
     r <- getParams
     evalState
       (
       do
          case (D.dipper_by_page_url (T.pack $ B.unpack $ request_dipper r) dr) of
             Nothing -> return $ return ()
             Just a  -> D.dipperT_individual_page_HandlerM a
                                          (dippers_from_request_string'' r dt)
       )
       s
       -}

   handler' :: [(D.Dipper,[String])] -> (D.Dipper,[MPC.PostT]) -> Routes -> Handler App App ()
   handler' dt dr s = do
     r <- getParams
     evalState
       (
       do let sel = (dippers_from_request_string'' r dt)
          case (find (\d->(fst dr)==d) sel) of
             Nothing -> return $ return ()
             Just a  -> D.dipperT_individual_page_HandlerM_tagged dr
                                          sel
                                          (request_tags_raw r)
       )
       s


   dippers_from_request_string'' tags p = D.dippers_from_request_string
           (
           request_tags_raw tags
           ) p





generate_dippers_tags_responseM :: State Routes [(ByteString, Handler App App ())]
generate_dippers_tags_responseM = do
   s@(Routes {dippers_tags=dippers_tags}) <- get
   return [(route , handler dippers_tags s)]
   where

   route = tagged_tag_link

   handler :: [(D.Dipper,[String])] -> Routes ->  Handler App App ()
   handler p s = do
     tags <- getParams
     evalState
       (
       D.dippersT_tagged_HandlerM (dippers_from_request_string' tags p)
                                  (D.give_all_used_tags p)
                                  (request_with_no_page_number tags)
                                  (fromMaybe 1 $ request_page_number tags)
                                  (links tags (dippers_from_request_string'' tags p))
       )
       s




   links tags d
      |(total_pages d) > 1 = map (tagged_node_link''
                                       (request_with_no_page_number tags)
                                  ) [1..total_pages d]
      |otherwise = []

   dippers_on_page_n :: Int -> D.Dippers -> D.Dippers
   dippers_on_page_n i d
      |i<=1 = take max_items_per_page d
      |(fromIntegral i)<=total_pages d = give_page i d
      |otherwise = take max_items_per_page $ reverse d
      where


   dippers_from_request_string' tags p =
           dippers_on_page_n (fromMaybe 1 $ request_page_number tags)
                             (dippers_from_request_string'' tags p)





   dippers_from_request_string'' tags p = D.dippers_from_request_string
           (
           request_with_no_page_number tags
           ) p






request tags = reverse $ drop 5 $ reverse $ B.unpack $ B.concat $ DMap.findWithDefault [] tagged_tag tags

request_tags_raw tags = B.unpack $ B.concat $ DMap.findWithDefault [] tagged_tag tags

request_dipper r = B.concat $ DMap.findWithDefault [] tagged_dipper r


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
total_items_archive d = length $ items_archive d
total_pages d = ceiling ((fromIntegral $ total_items d) / (fromIntegral max_items_per_page))
total_pages_archive d = ceiling ((fromIntegral $ total_items_archive d) / (fromIntegral max_items_per_page_archive))
items d = zip (concat $ map (replicate max_items_per_page) [1..]) d

items_archive p =
  zip (
       concat $ map (replicate max_items_per_page_archive) [1..]
      ) $ reverse p

give_page i d = snd $! unzip $ head $ drop (i-1) $ groupBy (\(n,_) (n1,_)-> n==n1) $ items d

give_page_archive i p =
  snd $! unzip $ head $ drop (i-1)
    $ groupBy (\(n,_) (n1,_)-> n==n1) $ items_archive p

give_page_archive_latest_first :: Int -> [b] -> [b]
give_page_archive_latest_first i p
  |((length $ page i)>5)||(max_items_per_page_archive<=5)  = page i
  |otherwise = (page i) ++ (page (i+1))
  where
    page i = snd $! unzip $ head $ drop (i-1)
               $ groupBy (\(n,_) (n1,_)-> n==n1) $ reverse $ items_archive p



------------------------------------------------------------------------------
-- | Compose all the Interpreted splices imported from the handler modules
allInterpretedSplices :: Monad n => Splices (I.Splice n)
allInterpretedSplices = mconcat []

------------------------------------------------------------------------------
-- | The application initializer.
app :: State Routes (SnapletInit App App)
app = do
    r<- routes
    return (makeSnaplet "app" "A snap demo application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ (set scInterpretedSplices allInterpretedSplices mempty)
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes r
    return $ App h s)
