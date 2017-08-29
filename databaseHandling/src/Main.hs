{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import qualified Control.Lens as L --hiding (element)
import Data.Maybe
import Control.Monad
import Control.Monad.State hiding (put, get)
import qualified Control.Monad.State as St
import Nodes
import System.Directory
import qualified System.FilePath.Posix as Fp
import Text.Parsec
--import qualified Data.Map.Strict as DMap
import Data.List
--import qualified Data.Maybe as M
import System.Environment
--import qualified Data.ByteString.Lazy as B
--import qualified Data.Text as T






data RunMode =
    SanityCheck
   |NewFromInlineDry
   |NewFromInlineExec
   --deriving (Show)


stringToRunMode :: String -> RunMode
stringToRunMode "SanityCheck" = SanityCheck
stringToRunMode "NewFromInlineDry" = NewFromInlineDry
stringToRunMode "NewFromInlineExec" = NewFromInlineExec
stringToRunMode x = SanityCheck


instance Read RunMode where
   readsPrec _ s = [(stringToRunMode s, "")]

data A = A {
    _mode :: Maybe RunMode
   ,_inlineFile :: Maybe FilePath
   ,_dbFile :: Maybe FilePath
   }
L.makeLenses ''A





main = do
  a<- getArgs
  j<- readFile $ unwords a

  evalStateT run $ A {
     _mode = Just $ read $ fromMaybe "" $ j ^? key "mode"  . _JSON
    ,_inlineFile = j ^? key "inlineFile"  . _JSON
    ,_dbFile = j ^? key "dbFile"  . _JSON
    }

  --print a
  {-case a of
    [] -> sanityCheck []
    a  -> do
             case (head a) of
               "--page" -> do
                              s <- processPage $ unwords $ tail a
                              nu<- node_from_string s
                              sanityCheck [(unwords $ tail a, nu)]
               _        -> do
                              nu<- node_from_name $ unwords a
                              sanityCheck [("new",nu)]
-}
run :: StateT A IO ()
run = do
   a<- St.get
   case a^.mode of
     Just SanityCheck -> liftIO $ sanityCheck []
     Just NewFromInlineDry -> do
        s <- liftIO $ processPage $ fromJust $ a^.inlineFile
        nu<- liftIO $ node_from_string s
        liftIO $ sanityCheck [(fromJust $ a^.inlineFile, nu)]
     Just NewFromInlineExec -> do
        liftIO $ putStrLn "Processing file for inline db entries"
        s <- liftIO $ processPage $ fromJust $ a^.inlineFile
        nu<- liftIO $ node_from_string s

        liftIO $ putStrLn "SanityCheck next =>[enter]"
        liftIO $ getLine
        liftIO $ sanityCheck [(fromJust $ a^.inlineFile, nu)]
        liftIO $ putStrLn "outputToNewDBFile next =>[enter]"
        liftIO $ getLine
        outputToNewDBFile s

        liftIO $ putStrLn "SanityCheck next =>[enter]"
        liftIO $ getLine
        liftIO $ sanityCheck []

        liftIO $ putStrLn "filterOutInlineDBEntries next =>[enter]"
        liftIO $ getLine
        filterOutInlineDBEntries


     _                 -> do
        nu<- liftIO $ node_from_name "new"
        liftIO $ sanityCheck [("new",nu)]





processPage f = do
   fr<- readFile f
   case (parse (many1 $ try directToDB) "(unknown)" fr) of
     Left e -> do
        putStrLn $ show e
        return ""
     Right p -> do
        let s = unlines ["[", unlines $ intersperse " , " $ entries p, "]"]
        putStrLn s
        putStrLn "---"
        return s
   where
      entries = map (\(n,l)-> entry n l)
   --return ()


heistEntry = do
   o<- string "${"
   e<- manyTill anyChar $ try $ lookAhead $ string "}"
   c<- string "}"
   return $ concat [o,e,c]

inlineEntry = do
   o<-string "{"
   e<-manyTill anyChar $ try $ lookAhead $ string "}"
   c<-string "}"
   return $ concat [o,e,c]
--inlineEntry = between (string "{") (string "}") $ many anyChar

directToDB = do
   manyTill anyChar $ try $ lookAhead $ directToDB''
   directToDB''

directToDB'' = do
   string "${"
   n<- manyTill anyChar $ try $ string "}"
   string "{"
   l<- manyTill anyChar $ try $ string "}"
   return (n,l)
{-
directToDB' = do
   manyTill anyChar (try (string "${"))
   n<- manyTill anyChar (try (string "}{"))
   l<- manyTill anyChar (try (string "}"))
   return (n,l)
-}

sanityCheck nu = do
  n <- nodes_io
  --mapM (mapM (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")) $ concat [n,nu]
  --mapM_ (putStrLn.show) $ nodesToNumberedList_fileRef $ concat [n,nu]

  putStrLn "Node database sanity check"
  putStrLn $ unlines $ map fst n
  putStrLn "Find same tags:"
  mapM_ (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")
    $ findSameTags_fileRef $ nodesToNumberedList_fileRef $ concat [n,nu]
  putStrLn "Find same links:"
  mapM_ (\x-> do mapM_ (putStrLn.show) x; putStrLn "---")
    $ findSameLinks_fileRef $ nodesToNumberedList_fileRef $ concat [n,nu]


outputToNewDBFile :: String -> StateT A IO ()
outputToNewDBFile s = do
   a<-St.get
   l<- liftIO $ getDirectoryContents "nodes"
   --print l
   let f = filter isNodeFile l
   case (elem (Fp.takeFileName $ fromJust $ a^.dbFile) f) of
      True -> liftIO $ putStrLn "dbFile exists"
      _    -> do
         liftIO $ writeFile (fromJust $ a^.dbFile) s



filterOutInlineDBEntries = do
   a<-St.get
   f<- liftIO $ readFile $ fromJust $ a^.inlineFile
   case (parse allButDBEntry "(unknown)" f) of
     Left e -> do
        liftIO $ putStrLn $ show e
        --return ()
     Right p -> do
        liftIO $ writeFile (concat [fromJust $ a^.inlineFile, ".bak"]) f
        liftIO $ writeFile (fromJust $ a^.inlineFile) p
        liftIO $ putStrLn p


allButDBEntry = do
   m<-many1 $ try allButDBEntry_p
   e<- many anyChar
   return $ concat [concat m, e]

allButDBEntry_keep = do
   b<- manyTill anyChar $ try $ lookAhead heistEntry
   he<- heistEntry
   return $ concat [b,he]



allButDBEntry_p = do
   k<-allButDBEntry_keep
   optional inlineEntry
   return k

nodes_io :: IO [(String, Nodes)]
nodes_io = do
   l<- getDirectoryContents "nodes"
   --print l
   let f = filter isNodeFile l
   --print f
   d <- mapM node_from_name_suffix $ number_from_node_name f
   return $ zip f d


nodesToNumberedList_LinksOnly n = map (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
         link = l }) = (i,l)



nodesToNumberedList_fileRef n =
  concatMap (\(n,f)-> f n)
     $ zip (concatMap (\(f,ns)-> zip (repeat f) ns) n)
     $ map node_to_list [1..]
  where
    node_to_list i
      (f,
       (Node {
          node = n
         ,link = l })
      ) = zip4 (repeat i) [f] n (repeat l)



nodesToNumberedList n = concatMap (\(n,f)-> f n) $ zip n $ map node_to_list [1..]
  where
    node_to_list i
      (Node {
          node = n
         ,link = l }) = zip3 (repeat i) n $ repeat l



findSameTags l = filter notSingle
  $ groupBy (\(_,n,_) (_,n',_) -> n==n')
  $ sortOn (\(_,n,_)-> n) l

findSameTags_fileRef l = filter notSingle
  $ groupBy (\(_,_,n,_) (_,_,n',_) -> n==n')
  $ sortOn (\(_,_,n,_)-> n) l

notSingle []  = False
notSingle [_] = False
notSingle  _  = True





findSameLinks l = filter notSingle
  $ groupBy (\(_,_,u) (_,_,u') -> u==u')
  $ sortOn (\(_,_,u)-> u) l

findSameLinks_fileRef l = filter notSingle
  $ groupBy (\(_,_,_,u) (_,_,_,u') -> u==u')
  $ sortOn (\(_,_,_,u)-> u) l


entry :: String -> String -> String
entry n l = unlines
  ["{"
  ,"  \"node_json\" : [\"" ++ n ++ "\"]"
  ," ,\"link_json\" : \"" ++ l ++ "\""
  ," }"
  ]
