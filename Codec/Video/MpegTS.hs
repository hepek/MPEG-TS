{-# LANGUAGE BangPatterns #-}

module Codec.Video.MpegTS where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import Data.ByteString.Lazy (empty)

import Data.Binary.Get
import Data.Word
import Data.Bits

import Control.Applicative hiding (empty)
import Control.Monad

import Debug.Trace

type Data         = BS.ByteString
type PID          = Word16
type PayloadStart = Bool
type ContC        = Word8
type SID          = Word8

data TS = TS
    { ts_pid   :: !PID
    , ts_pst   :: !PayloadStart
    , ts_contc :: !ContC
    , ts_ad    :: !(Maybe Adaptation)
    , ts_data  :: !(Maybe Data)
    } deriving (Show)

data PAT = PAT
    { pat_nextS       :: !Bool
    , pat_section     :: !Word8
    , pat_lastSection :: !Word8
    , pat_programs    :: ![PAT_Prog]
    } deriving (Show)

data PAT_Prog = PAT_Prog
    { prog_num :: !Word16
    , prog_pid :: !Word16
    } deriving (Show)

data PMT = PMT
    { pmt_prog_num    :: !Word16
    , pmt_nextS       :: !Bool
    , pmt_pcrPID      :: !Word16
    , pmt_programDesc :: !Data
    , pmt_progs       :: ![PMT_Prog]
    } deriving (Show)

data PMT_Prog = PMT_Prog
    { pmtp_streamType :: !StreamType
    , pmtp_elemPID    :: !Word16
    , pmtp_esInfo     :: !ESDescriptor
    } deriving (Show)

--type Adaptation   = BS.ByteString
data AdaptationFlags = AdaptationFlags
    { af_discont      :: !Bool
    , af_randomAccess :: !Bool
    , af_priority     :: !Bool
    , af_pcr          :: !Bool
    , af_opcr         :: !Bool
    , af_splice       :: !Bool
    , af_transportPrivateData :: !Bool
    , af_extension    :: !Bool
    }

defaultAdaptationFlags =
  AdaptationFlags False False False False False
                  False False False

instance Show AdaptationFlags where
  show (AdaptationFlags d r p pcr opcr spl tpd e) =
      (t d 'D')   : (t p 'P') : (t pcr 'C') : (t opcr 'O') : (t spl 'S') :
      (t tpd 'P') : (t e 'E') : []
          where
            t f c = case f of
                True  -> c
                False -> '-'

data Adaptation = Adaptation
    { ad_len      :: !Int
    , ad_flags    :: !AdaptationFlags
    , ad_pcr      :: !(Maybe Int)
    , ad_opcr     :: !(Maybe Int)
    , ad_splice   :: !Int
    } deriving (Show)

data PES = PES
    { pes_sid  :: !SID
    , pes_data :: !Data
    } deriving (Show)

data StreamType = VIDEO_MPEG1     | VIDEO_MPEG2    | AUDIO_MPEG1 | AUDIO_MPEG2
                | PRIVATE_SECTION | PRIVATE_DATA   | AUDIO_AAC   | VIDEO_MPEG4
                | AUDIO_LATM_AAC  | SYSTEMS_MPEG4_PES | SYSTEMS_MPEG4_SECTIONS
                | VIDEO_H264      | AUDIO_AC3      | AUDIO_DTS   | SUBTITLE_DVB
                | Other Word8
                  deriving (Show)

fromWord8 0x01  = VIDEO_MPEG1
fromWord8 0x02  = VIDEO_MPEG2
fromWord8 0x03  = AUDIO_MPEG1
fromWord8 0x04  = AUDIO_MPEG2
fromWord8 0x05  = PRIVATE_SECTION
fromWord8 0x06  = PRIVATE_DATA
fromWord8 0x0F  = AUDIO_AAC
fromWord8 0x10  = VIDEO_MPEG4
fromWord8 0x11  = AUDIO_LATM_AAC
fromWord8 0x12  = SYSTEMS_MPEG4_PES
fromWord8 0x13  = SYSTEMS_MPEG4_SECTIONS
fromWord8 0x1B  = VIDEO_H264
fromWord8 0x81  = AUDIO_AC3
fromWord8 0x8A  = AUDIO_DTS
fromWord8 0x100 = SUBTITLE_DVB
fromWord8 x     = Other x

data ESDescriptor = Reserved
                  | Video_stream_descriptor           | Audio_stream_descriptor          | Hierarchy_descriptor
                  | Registration_descriptor           | Data_stream_alignment_descriptor | Target_background_grid_descriptor
                  | Video_window_descriptor           | CA_descriptor                    | ISO_639_language_descriptor
                  | System_clock_descriptor           | Multiplex_buffer_utilization_descriptor
                  | Copyright_descriptor              | Maximum_bitrate_descriptor       | Private_data_indicator_descriptor
                  | Smoothing_buffer_descriptor       | STD_descriptor                   | IBP_descriptor
                  | MPEG                              | IOD_descriptor                   | SL_descriptor
                  | FMC_descriptor                    | External_ES_ID_descriptor        | MuxCode_descriptor
                  | FmxBufferSize_descriptor          | MultiplexBuffer_descriptor       | FlexMuxTiming_descriptor
                  | User_Private
                   deriving (Show)

esTag 0  =  Reserved
esTag 1  =  Reserved
esTag 2  =  Video_stream_descriptor
esTag 3  =  Audio_stream_descriptor
esTag 4  =  Hierarchy_descriptor
esTag 5  =  Registration_descriptor
esTag 6  =  Data_stream_alignment_descriptor
esTag 7  =  Target_background_grid_descriptor
esTag 8  =  Video_window_descriptor
esTag 9  =  CA_descriptor
esTag 10 =  ISO_639_language_descriptor
esTag 11 =  System_clock_descriptor
esTag 12 =  Multiplex_buffer_utilization_descriptor
esTag 13 =  Copyright_descriptor
esTag 14 =  Maximum_bitrate_descriptor
esTag 15 =  Private_data_indicator_descriptor
esTag 16 =  Smoothing_buffer_descriptor
esTag 17 =  STD_descriptor
esTag 18 =  IBP_descriptor
esTag 27 =  MPEG
esTag 28 =  MPEG
esTag 29 =  IOD_descriptor
esTag 30 =  SL_descriptor
esTag 31 =  FMC_descriptor
esTag 32 =  External_ES_ID_descriptor
esTag 33 =  MuxCode_descriptor
esTag 34 =  FmxBufferSize_descriptor
esTag 35 =  MultiplexBuffer_descriptor
esTag 36 =  FlexMuxTiming_descriptor
esTag x | (x >= 0x40 && x <= 0xFF) = User_Private
        | otherwise = Reserved

decodeESDescriptor :: Int -> Get ESDescriptor
decodeESDescriptor l = do
    tag <- getWord8
    skip (l-1)
    return (esTag tag)

decodeTS :: Get TS
decodeTS = do
    checkSyncByte
    (ps, pid) <- parsePID
    (ad, cc)  <- parseAD
    let pack = TS pid ps cc
    case ad of
       0 -> fail (show (pack Nothing Nothing) ++ ": wrong adaptation field code")
       2 -> do af <- decodeAdaptation
               skip (184-(ad_len af)-1)
               return$ pack (Just af) Nothing
       3 -> do af <- decodeAdaptation
               d  <- getByteString (184-(ad_len af)-1)
               return$ pack (Just af) (Just d)
       _ -> do d <- getByteString 184
               return$ pack Nothing (Just d)
     where
        checkSyncByte =
          do sync <- getWord8
             when (sync /= 0x47) (fail$ "bad sync byte " ++ show sync) -- checkSyncByte
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
        when (pspa /= 0x0000 || pspb /= 0x01) (fail $ "bad psp sync bytes" ++ show pspa ++ " " ++ show pspb)
      getOptionalPES = do
        h <- getWord16be
        l <- fromIntegral <$> getWord8
        getByteString l

last10 = ((2^10-1).&.)
last13 = ((2^13-1).&.)

decodePAT :: Bool -> Get PAT
decodePAT start = do
  when start (skip 1)
  tableID <- getWord8
  when (tableID /=0x00)
    (fail "PAT: wrong tableID")
  n16 <- getWord16be
  let len = last10 n16
  skip 2
  a       <- getWord8
  sn      <- getWord8
  lastSn  <- getWord8
  let progN = (len-9) `div` 4
  progs   <-
    forM [1..progN]
    (\x -> PAT_Prog <$> getWord16be <*> (last13 <$>  getWord16be))
  skip 4 -- CRC32
  return $ PAT (testBit a 0) sn lastSn progs

decodePMT :: Bool -> Get PMT
decodePMT start = do
  when start (skip 1)
  tableID  <- getWord8
  when (tableID /= 0x02)
    (fail "PMT: wrong tableID")
  n16      <- getWord16be
  let len = last10 n16
  prog_num     <- getWord16be
  skip 3
  pcr_pid      <- last13 <$> getWord16be
  pinfo_len    <- last10 <$> getWord16be
  program_desc <- getByteString (fromIntegral pinfo_len)

  progs <-
    forM [1..2] (\_ -> do
      stype    <- fromWord8 <$> getWord8
      pid      <- last13 <$> getWord16be
      esInfoL  <- last10 <$> getWord16be
      esDesc  <- decodeESDescriptor (fromIntegral esInfoL)
      return$ PMT_Prog stype pid esDesc)
  skip 4 --CRC32

  return PMT
             {  pmt_prog_num    = prog_num
             , pmt_nextS       = False
             , pmt_pcrPID      = pcr_pid
             , pmt_programDesc = program_desc
             , pmt_progs       = progs
             }

maybeParse False _     = return Nothing
maybeParse True parser = do
    a <- parser
    return (Just a)


decodeAdaptation :: Get Adaptation
decodeAdaptation = do
   len    <- fromIntegral <$> getWord8
   cursor <- bytesRead
   flags <-
       if (len == 0)
       then return defaultAdaptationFlags
       else decodeAdaptationFlags
   pcr    <- decodePCR (af_pcr  flags)
   opcr   <- decodePCR (af_opcr flags)
   splice <- case (af_splice flags) of
                False -> return 0
                True  -> fromIntegral <$> getWord8
   cursor2 <- bytesRead
   let consumed = fromIntegral (cursor2 - cursor)
   skip (len-consumed)
   return $ Adaptation len flags pcr opcr splice
    where
      decodePCR False = return Nothing
      decodePCR True  = do
        up <- getWord32be
        --lw <- getWord16be TODO: extension n stuff
        skip 2
        let ret = 2 * fromIntegral up
        return (Just ret)

decodeAdaptationFlags :: Get AdaptationFlags
decodeAdaptationFlags = do
    flags <- getWord8
    let test = testBit flags
    return $ AdaptationFlags (test 7) (test 6) (test 5) (test 4)
                             (test 3) (test 2) (test 1) (test 0)


parseListOf parser bytestring offset =
    let (x, rest, i) = runGetState parser bytestring offset
    in
    if rest == empty
    then x:[]
    else x:(parseListOf parser rest i)

parseOffsetMap parser bytestring offset =
    let (x, rest, i) = runGetState parser bytestring offset
    in
    if rest == empty
    then (x,i):[]
    else (x,i):(parseOffsetMap parser rest i)

collectTS = parseListOf decodeTS

collectTSOff = parseOffsetMap decodeTS
