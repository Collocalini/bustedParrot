-----------------------------------------------------------------------------
--
-- Module      :  InsertLinks
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

module InsertLinks (
insertLinks
) where



import           Heist
import qualified Heist.Interpreted as I
import Data.Monoid
import qualified Data.Maybe as M
import qualified Data.Map as Dm
import Nodes

insertLinks ::  Monad n => Maybe Node_map -> Splices (I.Splice n)
insertLinks nm
   |M.isJust nm = mconcat $ map splice list_of_key_value_pairs
   |otherwise = "test_link" ## I.textSplice ""
   where
   list_of_key_value_pairs = Dm.toList $ M.fromJust nm
   splice (t,l) = t ## (I.textSplice l)










