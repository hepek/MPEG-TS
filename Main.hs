module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS

import Data.Binary.Get
import Data.Word
import Data.Bits

import Control.Applicative hiding (empty)
import Control.Monad

import Data.List
import Data.Maybe
import System.Environment
import System.Directory (removeFile)
import System.IO

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy as NL

import Codec.Video.MpegTS

filterPID pid = filter ((pid==).ts_pid)

printUsage = do
  name <- getProgName
  hPutStrLn stderr $ unlines 
    [name ++ " - a program for MPEGTS stream analysis."
    ,"Usage: " ++ name ++ " info <FILE>                        #to view stream info of a file"
    ,"       " ++ name ++ " info <HOST> <PORT>                 #to view stream info of a UDP stream"
    ,"       " ++ name ++ " demux pid <SOURCEFILE> <DESTFILE>  #to demux a file"]

printInfoFile = printInfo <=< BL.readFile
printInfoUDP  = printInfo <=< readUDP 
  where
    readUDP (host,port) = withSocketsDo $ do
      serveraddr <- head <$> getAddrInfo Nothing (Just host) (Just port)
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      putStrLn $ "connecting to " ++ host ++ ':':port
      connect sock (addrAddress serveraddr)
      NL.getContents sock      

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

main = do
  args <- getArgs
  case args of
    ["info", fileName]                    -> printInfoFile fileName
    ["info", host, port]                  -> printInfoUDP  (host,port)
    ["demux", pid, sFileName, dFileName]  -> selectPID (read pid) sFileName dFileName
    _ -> printUsage