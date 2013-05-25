
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS

import Data.Binary.Get
import Data.Word
import Data.Bits

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Arrow

import Data.List
import Data.Maybe
import System.Environment
import System.Directory (removeFile)
import System.IO

import Codec.Video.MpegTS

import Numeric

filterPID pid = filter ((pid==).ts_pid)

printUsage = do
  name <- getProgName
  hPutStrLn stderr $ unlines
    [name ++ " - a program for MPEGTS stream analysis."
    ,"Usage: " ++ name ++ " info          <FILE>               #to view stream info of a file"
    ,"       " ++ name ++ " adaptation    <FILE>               #to view stream adaptation fields"
    ,"       " ++ name ++ " discont <PID> <FILE>               #to view stream discontinuities"
    ,"       " ++ name ++ " hasDiscont    <FILE>               #to check for discontinuities in any program"
    ,"       " ++ name ++ " demux pid <SOURCEFILE> <DESTFILE>  #to demux a file"]

printInfoFile = printInfo <=< BL.readFile
printInfo bytes = do
  let tspackets = collectTS bytes 0
  let patTS     = head $ filterPID 0 tspackets
  let pat       = runGet (decodePAT (ts_pst patTS))
                          (BL.fromChunks [fromJust.ts_data$ patTS])

  forM_ (pat_programs pat)
    (\(PAT_Prog num pid) -> do
        putStrLn$ "Program: " ++ (show num)
        let pmtTS = filterPID pid tspackets
        let pmt   = runGet (decodePMT $ ts_pst.head $ pmtTS) (BL.fromChunks (map (fromJust.ts_data) pmtTS))
        printPMT pmt)

     where
       printPMT (PMT _  _ pcrpid _ progs) = do
         putStrLn$ "\tPCR: " ++ show pcrpid
         forM progs
           (\(PMT_Prog st pid info) -> do
             putStrLn$ "\tStream PID: "    ++ show pid
             putStrLn$ "\t\tStream Type: " ++ show st
             putStrLn$ "\t\tDescription: " ++ show info)

showWOData ts = do
  putStrLn$ "ts_pid: "   ++ (show $ ts_pid ts)
  putStrLn$ "ts_pst: "   ++ (show $ ts_pst ts)
  putStrLn$ "ts_contc: " ++ (show $ ts_contc ts)
  putStrLn$ "ts_ad: "    ++ (showAD $ ts_ad ts)
  putStrLn "------------------------"

showAD Nothing   = "Nothing"
showAD (Just ad) = "len: " ++ show (ad_len ad)  ++ " flags: " ++ show (ad_flags ad) ++ "\tpcr: "
                           ++ show (ad_pcr ad) ++ "\topcr: " ++ show (ad_opcr ad) ++ "\tspl: " ++ show (ad_splice ad)

printAdaptation = printInfoAd <=< BL.readFile
 where
  printInfoAd bytes = do
   let tspackets = filter (isJust.ts_ad)(collectTS bytes 0)
   forM_ tspackets showWOData

printDisconts pid = printInfoDisconts pid <=< BL.readFile
 where
    printInfoDisconts pid bytes = do
      mapM_ (\(ts, off) -> do
                putStrLn$ "0x" ++ (showHex off ":")
                showWOData ts)
           (filter (\(ts,_) -> (af_discont.ad_flags) (fromJust (ts_ad ts))) $
            filter (\(ts,_) -> (isJust.ts_ad $ ts) && (ts_pid ts == pid)) $ (collectTSOff bytes 0))

discontinuities pids tss = filter (\ts -> (af_discont.ad_flags) (fromJust (ts_ad ts))) $ filter (\ts -> (isJust.ts_ad $ ts) && (ts_pid ts `elem` pids)) $ tss

selectPID pid srcFileName destFileName = do
  bytes <- BL.readFile srcFileName
  let packets = (filterPID pid $ collectTS bytes 0)
  case packets of
    [] -> error $ "PID " ++ (show pid) ++ " not present in the stream"
    _  ->
      forM_ packets (\x-> case (ts_data x) of
                        Just datum -> BS.appendFile destFileName datum
                        Nothing    -> return ())
  return ()

hasDiscont = hasDiscont' <=< BL.readFile
 where
  hasDiscont' bytes = do
    let tspackets     = collectTS bytes 0
    let patTS     = head $ filterPID 0 tspackets
    let pat       = runGet (decodePAT (ts_pst patTS))
                             (BL.fromChunks [fromJust.ts_data$ patTS])
    pcrs <- forM (pat_programs pat)
       (\(PAT_Prog num pid) -> do
         let pmtTS = filterPID pid tspackets
         let pmt   = runGet (decodePMT $ ts_pst.head $ pmtTS) (BL.fromChunks (map (fromJust.ts_data) pmtTS))
         return $ pmt_pcrPID pmt)

    if (null $ discontinuities pcrs tspackets)
      then
        return ()
      else
        error "discontinuities found"

main = do
  args <- getArgs
  case args of
    ["adaptation", fileName]              -> printAdaptation fileName
    ["discont", pid, fileName]            -> printDisconts (read pid)  fileName
    ["hasDiscont", fileName]              -> hasDiscont fileName
    ["info",    fileName]                 -> printInfoFile   fileName
    ["demux", pid, sFileName, dFileName]  -> selectPID (read pid) sFileName dFileName
    _ -> printUsage
