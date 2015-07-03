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
  Dippers
 ,dippersT_h_io
 ,dippersT_Handler
) where

import Data.Aeson
import GHC.Generics
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import qualified System.FilePath.Posix as Fp
import Data.List
import qualified Data.Maybe as DM
import Data.Monoid
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------




data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,comment :: Maybe T.Text
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





dippersT_h_io :: IO Dippers
dippersT_h_io = do
   l<- getDirectoryContents "dippers"
   d <- mapM s2p $ ff $ filter f l
   return $ concat d
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




give_dipper :: String -> Dipper_json -> Maybe Dipper
give_dipper s dj@(Dipper_json {miniature_json = _
                             ,name_json      = _
                             ,url_json       = ""
                             ,comment_json   = _
                             }) = Nothing

give_dipper s dj@(Dipper_json {miniature_json = m
                             ,name_json      = n
                             ,url_json       = u
                             ,comment_json   = c
                             }) =
   Just $ Dipper {
     miniature = maybe_miniature
    ,name      = maybe_name
    ,page_url  = T.pack $ Fp.replaceExtension (T.unpack u) ".html"
    ,url       = u
    ,comment   = maybe_comment
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




dippersT_Handler :: Dippers -> Handler App App ()
dippersT_Handler p = renderWithSplices "dipper/dipper_entries"
   ("entries" ##
   (I.mapSplices $ I.runChildrenWith . splicesFrom_dippers) p
   )





splicesFrom_dippers :: Monad n => Dipper -> Splices (I.Splice n)
splicesFrom_dippers t = do
   mconcat $ [
     "dipper_url"        ## I.textSplice $ url       t
    ,"dipper_miniature"  ## I.textSplice $ M.fromJust $ miniature t
    ]










