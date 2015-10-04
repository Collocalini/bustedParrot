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

module Dipper_common (
 Dipper(..)
,Dippers
) where

import qualified Data.Text as T




data Dipper = Dipper {
  miniature :: Maybe T.Text
 ,name :: Maybe T.Text
 ,page_url :: T.Text
 ,url :: T.Text
 ,url_raw :: T.Text
 ,comment :: Maybe T.Text
 ,isVertical :: Bool
} deriving (Show,Eq)

type Dippers = [Dipper]




















