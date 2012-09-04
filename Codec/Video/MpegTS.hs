module MpegTS where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word
import Control.Applicative
import Data.Bits
import Control.Monad

type Data         = BS.ByteString
type Adaptation   = BS.ByteString
type PID          = Word16
type PayloadStart = Bool
type ContC        = Word8
type SID          = Word8

data TS = TS
           { ts_pid   :: PID
           , ts_pst   :: PayloadStart
           , ts_contc :: ContC
           , ts_ad    :: Maybe Adaptation
           , ts_data  :: Maybe Data
           }

instance Show TS where
  show (TS id ps cc Nothing Nothing) = undefined
  show (TS id ps cc Nothing _) = "TS\t" ++ show ps ++ "\t" ++ show cc ++ "\t" ++ show id ++ "\t_ d"
  show (TS id ps cc _ Nothing) = "TS\t" ++ show ps ++ "\t" ++ show cc ++ "\t" ++ show id ++ "\ta _"
  show (TS id ps cc _ _)       = "TS\t" ++ show ps ++ "\t" ++ show cc ++ "\t" ++ show id ++ "\ta d"


decodeTS :: Get TS
decodeTS = do
    checkSyncByte
    (ps, pid) <- parsePID
    (ad, cc)  <- parseAD
    let pack = TS pid ps cc
    case ad of
       0 -> fail "wrong adaptation field"
       1 -> do d <- getByteString 184
               return$ pack Nothing (Just d)
       2 -> do af <- getByteString 184
               return$ pack (Just af) Nothing
       3 -> do l  <- fromIntegral <$> getWord8
               af <- getByteString l
               d  <- getByteString (184-l-1)
               return$ pack (Just af) (Just d)
     where
        checkSyncByte = 
          do sync <- getWord8
             when (sync /= 0x47) (fail "bad sync byte")
        parsePID = 
          do chunk <- getWord16be
             let ps  = testBit chunk 14
                 pid = 0x1FFF .&. chunk
             return (ps, pid)
        parseAD =
          do byte <- getWord8
             let ad = (shiftR (0x30 .&. byte) 4)
                 cc = (0x07 .&. byte)
             return (ad,cc)

data PES = PES { pes_sid :: SID
               , pes_data :: Data
               } deriving (Show)

decodePES :: Get PES
decodePES = do
  checkPSP
  sid <- getWord8
  len <- fromIntegral <$> getWord8
  optional <- getOptionalPES
  d <- case len of
         0 -> getByteString len
         _ -> getByteString len
  return$ PES sid d

    where
      checkPSP = do
        pspa <- getWord16be
        pspb <- getWord8
        when (pspa /= 0x0000 || pspb /= 0x01) (fail "bad sync byte")
      getOptionalPES = do
        h <- getWord16be
        l <- fromIntegral <$> getWord8
        getByteString l


data PAT = PAT 
           { pat_nextS       :: Bool
           , pat_section     :: Word8
           , pat_lastSection :: Word8
           , pat_programs    :: [Program]
           } deriving (Show)                      

data Program = Program 
               { prog_num :: Word16
               , prog_pid :: Word16
               } deriving (Show)
                         
data PMT = PMT
           { pmt_sectionLength :: Word16
           , pmt_nextS :: Bool
           , pmt_section     :: Word8
           , pmt_lastSection :: Word8
           , pmt_pcrPID      :: Word16
           , pmt_pinfoLength :: Word16
           , pmt_programDesc :: Data
           , pmt_progs       :: [PMT_Prog]
           } deriving Show

--data PMT_Prog = PMT_Prog =

decodePAT :: Bool -> Get PAT
decodePAT start = do
  when start (skip 1)
  tableID <- getWord8
  when (tableID /=0x00) 
    (fail "PAT: wrong tableID")
  n16     <- getWord16be
  let len = n16 .&. last10
  skip 2
  a       <- getWord8
  sn      <- getWord8
  lastSn  <- getWord8
  let progN = (len-9) `div` 4
  progs   <-
    forM [1..progN]  -- todo check
    (\x -> Program <$> getWord16be <*> (((2^13-1) .&.) <$>  getWord16be))
  skip 4 -- CRC32
  return $ PAT (testBit a 0) sn lastSn progs
    where last10 = (2^10-1)