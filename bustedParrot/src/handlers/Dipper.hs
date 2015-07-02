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

) where


import qualified Data.Text as T
import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import System.Directory
import qualified Filesystem.Path as Fp
import Data.List
import qualified Data.Maybe as DM
import Data.Monoid
import Control.Monad
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
  miniature :: T.Text
 ,name :: T.Text
 ,url :: T.Text
 ,comment :: T.Text
} deriving (Show,Generic)


type Dippers_json = [Dipper_json]
type Dippers = [Dipper]


instance FromJSON Dipper_json
instance ToJSON Dipper_json

instance FromJSON Dippers_json
instance ToJSON Dippers_json



dippersT_h_io :: IO Dippers
dippersT_h_io = do
   l<- getDirectoryContents "dippers"
   d <- mapM s2p $ ff $ filter f l
   concat d
   where
     f :: FilePath -> Bool
     f ('d':'i':'p':'p':'e':'r':'_':_) = True
     f _ = False

     s2p :: String -> IO Dippers
     s2p s = do
       d <- eitherDecode <$> getJSON ("dippers/dipper_" ++ s ++ ".json"):: IO (Either String Dippers)
       case d of
        Left err -> putStrLn err
        Right dl -> return $ catMaybes $ map (give_dipper s) dl


     ff :: [FilePath] -> [String]
     ff fp = map (n . (drop 7)) fp
       where
         n x = (take ((length x)-5) ) x




give_dipper :: (Dipper_json, String) -> Maybe Dipper
give_dipper (dj@(Dipper_json {miniature = _
                             ,name      = _
                             ,url       = ""
                             ,comment   = _
                             })     ,   s   ) = Nothing

give_dipper (dj@(Dipper_json {miniature = m
                             ,name      = n
                             ,url       = u
                             ,comment   = c
                             })     ,   s   ) =
   Just $ Dipper {
     miniature = maybe_miniature
    ,name      = maybe_name
    ,page_url  = (Fp.basename u) ++ ".html"
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













