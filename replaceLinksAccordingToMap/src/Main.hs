{-# LANGUAGE OverloadedStrings , ScopedTypeVariables #-}

import qualified Data.Map.Strict as DMap
import Data.List
import Data.Maybe
import System.Environment
--import Data.Conduit
--import qualified Data.Conduit.List as CL
--import Control.DeepSeq
--import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as B
import qualified Data.Text as T
--import qualified Data.Text.Encoding as Te
import Control.Exception
import Text.XmlHtml
--import Text.Blaze.Html.Renderer.String
--import Text.Blaze.Html
--import Text.Blaze
import Blaze.ByteString.Builder
--import Text.Blaze.Internal
--Data.Text.Internal.Builder


main = do
  a<- getArgs
  --print a
  f<- readFile $ head a
  rm<- readFile $ head $ drop 1 a
  --print (replace_map $ lines rm)
  mapM_ (processFile (replace_map $ lines rm)) $ filter isHTML $ lines f
  

replace_map lst = DMap.fromList $ map fromJust $ filter isJust $ map (step1.words) lst
  where
   -- step1 :: [B.ByteString] -> Maybe (T.Text, T.Text)
    step1 [h,n] = Just (T.pack n, T.pack h)
    step1 _     = Nothing




replaceInNode rm n
  |hasAttribute href  n = r href
  |hasAttribute src   n = r src
  |hasAttribute data_ n = r data_
  |otherwise = n
  where
    r x = setAttribute x
                       (replaceIfNeeded
                          (fromJust $ getAttribute x n)
                           rm
                       )-------------------------------------
                       n
                       
    href    = "href"
    src     = "src"
    data_   = "data"
  



replaceIfNeeded o rm
  |(isHTML $ T.unpack o)||(isCSS $ T.unpack o)  = o
  |otherwise = --ipfs_link $ DMap.findWithDefault o o rm
               step1 $ DMap.lookup o rm
  where
    step1 Nothing  = o
    step1 (Just l) = ipfs_link l
            




   
--findAndReplace_all _ []        = []
--findAndReplace_all rm (n:rest) = r
--findAndReplace_all :: DMap.Map -> [Node]
findAndReplace_all _ []  = []
findAndReplace_all' rm s =
  map (c'.singleNode) s
  where
    c n = findAndReplace_all' rm $ childNodes n
    
    c' n@(Element {}) = (\n-> n { elementChildren=c n}) n
    c' n = n
    singleNode n =
      replaceInNode rm n
  --  r = map (replaceInNode rm) s
  --  c = map ((findAndReplace_all rm).childNodes.(replaceInNode rm)) s)


    
{-
findAndReplace (n,h) s = T.replace r' (ipfs_link h) $ T.replace r w s
  where
    r1 = c "href=" "\'" n
    r2 = c "href=" "\"" n
    r3 = c 
    w  = c "href=" "'" $ ipfs_link h
    c p x s = T.concat [p ,x ,s ,x]
    

findAndReplace_all rm s = foldl' fr s $ map findAndReplace rm
  where
    fr s f = f s 
-}



ipfs_link h = T.append "http://127.0.0.1:8080/ipfs/" h


processFile rm f = do
  s<- try $ B.readFile f
  case s of
    Left (e :: SomeException) -> return () --print e
    Right s -> do
      case (parseHTML f s) of
        Left s -> putStrLn s
        Right d@(HtmlDocument {docContent=s}) -> do
          --let fr = T.unpack $ findAndReplace_all rm s
          let fr = --renderHtml
                   --  $ toHtml
                   --  $ toMarkup
                   --  $ textBuilder
                   --B.unpack
                     toByteString
                     $ renderHtmlFragment UTF8 
                     -- T.unpack $ T.unlines $ map nodeText
                     $ findAndReplace_all' rm s
                    -- $ render
                    -- $ (\d-> d { docContent=findAndReplace_all' rm s}
                    --   ) d
          B.writeFile ("../ipfs_version/" ++ (drop 2 f)) fr





isHTML :: String -> Bool
isHTML [_] = False
isHTML [_,_] = False
isHTML [_,_,_] = False
isHTML [_,_,_,_] = False
isHTML s 
  |".html" == e = True
  |otherwise = False
   where
     e = reverse $ take 5 $ reverse s

isCSS :: String -> Bool
isCSS [_] = False
isCSS [_,_] = False
isCSS [_,_,_] = False
isCSS [_,_,_,_] = False
isCSS s 
  |".css" == e = True
  |otherwise = False
   where
     e = reverse $ take 4 $ reverse s
