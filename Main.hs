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

import Codec.Video.MpegTS

filterPID pid = filter ((pid==).ts_pid)

printUsage = do
  name <- getProgName
  putStrLn$ "Usage: " ++ name ++ " info <FILE>"
  putStrLn$ "       " ++ name ++ " select pid <SOURCEFILE> <DESTFILE>"

printInfo fileName = do
  bytes <- BL.readFile fileName
  let tspackets = collectTS bytes 0
  let patTS     = head $ filter ((0==).ts_pid) tspackets
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
  mapM_ (\x-> case (ts_data x) of
                  Just datum -> BS.appendFile destFileName datum
                  Nothing    -> return ())
       (filterPID pid $ collectTS bytes 0)
  return ()

main = do
  args <- getArgs
  case args of 
    [command, fileName] ->
      case command of
        "info"   -> printInfo fileName
        _        -> printUsage
    [command, pid, sFileName, dFileName] ->
      case command of
        "select"  -> selectPID (read pid) sFileName dFileName
        _         -> printUsage
    _ -> printUsage