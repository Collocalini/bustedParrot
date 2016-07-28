-----------------------------------------------------------------------------
--
-- Module      :  Page_common
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
module Page_common (
 PageT(..)

) where


import           Heist
--import GHC.Generics (Generic)
--import Control.DeepSeq


data PageT = PageT {
 pageT :: Template
,name :: String
} --deriving (Generic, NFData)
