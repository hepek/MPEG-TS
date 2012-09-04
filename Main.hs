module Main (main) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (empty)
import Data.Binary.Get
import Data.Word
import Control.Applicative hiding (empty)
import Data.Bits

import Data.List.Split
import Data.List
import Data.Maybe

import Data.Set as S (fromList, toList)

import MpegTS

-- selectP :: PID -> [TS] -> [BS.ByteString]
-- selectP pid xs = concatMap (map getData) $ splitStream $ filter ((==pid).ts_pid) xs
--    where splitStream = split $ whenElt getData
--          getData (TS _ _ _ _ Nothing )  = BS.empty
--          getData (TS _ _ _ _ (Just d)) = d
         
parseListOf parser bytestring offset = 
  let (x, rest, i) = runGetState parser bytestring offset
  in
    if rest == empty
       then x:[]
       else x:(parseListOf parser rest i)

collectTS = parseListOf decodeTS

--uniq :: [a] -> [a]
--uniq = S.toList . S.fromList

main = do
  bytes <- BL.readFile "HD.mpg"
  let tspackets = collectTS bytes 0
  let pmt = map fromJust $ map ts_data $ filter ((32==).ts_pid) $ tspackets
  print $ runGet (decodePMT True) (BL.fromChunks [(head pmt)])
  return tspackets

main2 = do
  bytes <- BL.readFile "HD.mpg"
  return  $ map ts_data $ filter ((0x0==).ts_pid) $ collectTS bytes 0
--  mapM_ print $ uniq $ map ts_pid $ collectTS bytes 0
-- mapM_ print $ (map (\x-> (head x, length x))).group.sort $ map getPID $ collectTS bytes 0

--test = do pd <- main
--          let (pat, _, _) = runGetState (decodePAT True) (BL.fromChunks [pd]) 0
--          print 
--pat
