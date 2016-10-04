

import Data.List
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B


main = do
  a<- getArgs
  r1<- readFile $ head a
  r2<- readFile $ head $ drop 1 a
  let r3 = head $ drop 2 a
  writeFile r3 $ unlines $ intersperse " " $ map (\(l,r)-> entry l r) $ zip (lines r1) (lines r2)
  



entry n l = unlines
   [",{"
   ,"  \"node_json\" : [\"" ++ n ++ "\"]"
   ," ,\"link_json\" : \"" ++ l ++ "\""
   ," }"
   ]














