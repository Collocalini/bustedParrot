-----------------------------------------------------------------------------
--
-- Module      :  Dipper_common
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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}



module Dipper_common (
 Dipper(..)
,Dippers
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq


data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,url_raw :: T.Text
 ,comment :: Maybe T.Text
 ,isVertical :: Bool
} deriving (Show,Eq,Generic, NFData)

type Dippers = [Dipper]




















