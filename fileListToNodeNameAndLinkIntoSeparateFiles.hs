{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified System.FilePath.Posix as Fp

main = do
   a<- getArgs
   j<- readFile $ unwords a
   let (nodeNames, links) = unzip $ map line2DBentry $ lines j
   writeFile ("nodeNames_" ++ unwords a) $ unlines nodeNames
   writeFile ("links_"     ++ unwords a) $ unlines links
   
   
line2DBentry s = (nodeName s, link s)
   where
   nodeName s = Fp.takeFileName s
   link     s = s     
   

