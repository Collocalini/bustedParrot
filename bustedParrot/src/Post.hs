-----------------------------------------------------------------------------
--
-- Module      :  Post
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

module Post (
post_Handler
) where

--import qualified Data.Text as T
--import qualified Text.XmlHtml as TT
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
{-import System.Directory
import Data.List
import Data.Monoid
import Control.Monad-}
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------
import Main_page



post_Handler :: PostT -> Handler App App ()
post_Handler p = renderWithSplices "post/post_base"
   (
   splicesFrom_post_h p
   )



splicesFrom_post_h :: Monad n => PostT -> Splices (I.Splice n)
splicesFrom_post_h t = do
  "post_h"  ## return (postT t)


