

import Data.List
import System.Process
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B


main = do
  a<- getArgs
  r<-run (head a) $ B.pack ""
  B.putStrLn $ B.unlines r

  --mapM_ B.putStrLn r


ipfs_ls hash = "ipfs ls " ++ hash

ipfs_ls' hash = do
  --(runCommand $ "echo \"\" > " ++ output) >>= waitForProcess
  (runCommand $ ipfs_ls hash ++ " > " ++ output) >>= waitForProcess
  s <- B.readFile output
  return $ B.lines s
  where
    output = "ipfs_ls_hashes"


getFolders :: [B.ByteString] -> [B.ByteString]
getFolders s = filter isFolder s


run hash prep
  |isHash hash = do
     x<- ipfs_ls' hash
     case (all lineIsSane x) of
       True -> do
               y<- mapM (\x-> run (B.unpack      $ strip_to_hash x)
                                  (B.append prep $ strip_to_name x)
                        ) $ getFolders x
               return $
                 (map (output.split_to_hash_and_name) x)
                 --x
                 -- ++ [B.pack $ show $ all lineIsSane x]
                 -- ++ [B.pack "--------------"]
                 ++ (concat y )
       False -> return $
                 [B.pack "!!!!!!!!!!!!!!!!"]
                 ++ x
                 ++ [B.pack "!!!!!!!!!!!!!!!!"]
     
     

  |otherwise = return [B.pack (hash ++ " !!!!!!!!" )]
  where
    output (h,n) = B.append h $ B.append (B.pack " ") $ B.append prep n

--prepend p s = B.append p $ strip_to_name s
strip_to_hash s = head $ B.words s
strip_to_name s = B.unwords $ drop 2 $ B.words s
split_to_hash_and_name s = (strip_to_hash s, strip_to_name s)

isFolder :: B.ByteString -> Bool
isFolder s
  |lastChar == '/' = True
  |otherwise       = False
     where
       lastChar = B.last $ last $ B.words s 


lineIsSane :: B.ByteString -> Bool
lineIsSane s = step1 w
  where
    step1 :: [B.ByteString] -> Bool
    step1 (hash:size:name) = (isHash $ B.unpack hash)&&(isNumber size)&&((length name)>0)
    step1 _ = False
    
    isNumber s =
      ((length $ intersect "0123456789" $ B.unpack s)>0)
      &&
      ((length $ intersect "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM{}:\"<>?,./[];'" $ B.unpack s)==0)
    w = B.words s


isHash h
  |(l)&&(qm h) = True
  |otherwise     = False
  where
    l
      |(length h) == 46 = True
      |otherwise        = False
       
    qm ('Q':'m':_) = True
    qm _           = False
      



















