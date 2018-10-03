{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified System.FilePath.Posix as Fp

main = do
   a<- getArgs
   j<- readFile $ unwords a
   putStrLn $ unlines $ map line2DBentry $ lines j
   
   
line2DBentry s = (nodeName s) ++ (link s)
   where
   nodeName s = "${" 
                  ++ Fp.takeFileName s
                  ++ "}"
   link     s = "{" ++ s ++ "}"     
   

