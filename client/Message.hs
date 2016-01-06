{-# LANGUAGE RecordWildCards #-}
module Message (
  
               ) where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Type as ST

import Control.Monad
import Data.Char (chr)


       -- --------------------
       -- socks5 messages ----
       -- --------------------
       
instance Binary (ST.InitMsg) where
  put (ST.InitMsg n m) = do
    putWord8 5
    putWord8 $ fromIntegral n
    mapM_ putWord8Enum m
  get = do
    getWord8                    -- version
    nmethods <- getWord8 >>= return . fromEnum . toInteger
    methods <- replicateM nmethods getWord8Enum
    return (ST.InitMsg nmethods methods)

instance Binary (ST.ResponseInitMsg) where
  put (ST.ResponseInitMsg m) = do
    putWord8 5
    putWord8Enum m
  get = do
    getWord8
    method <- getWord8Enum
    return $ ST.ResponseInitMsg method
    
instance Binary (ST.RequestMsg) where
  put ST.RequestMsg{..} = do
    putWord8 5
    putWord8Enum cmd
    putWord8 0
    putWord8Enum reqAtyp
    -- putWord8 $ fromIntegral $ fromEnum reqAtyp
    case (reqAtyp, dstAddr) of
      (ST.IPV4, ST.IPDst addr) -> putWord32be addr -- addr 本身是大端的，但是rfc的意思应该是4个
                                                  -- 字节分开的，也就是整体来看是小端的(?)
      (ST.DOMAINNAME, ST.DOMAINNAMEDst addr) -> do
        putWord8 $ fromIntegral $ length addr
        mapM_ put addr
      (ST.IPV6, _) -> error "not support ipv6 yet"
      _ -> error "request [atyp] not match with [dstAddr]"
    putWord16be $ fromIntegral dstPort
    
  get = do
    getWord8
    cmd <- getWord8Enum
    getWord8
    reqAtyp <- getWord8Enum
    dstAddr <- case reqAtyp of
      ST.IPV4 -> getWord32be >>= return . ST.IPDst
      ST.DOMAINNAME -> do
        len <- fromEnum <$> getWord8
        ST.DOMAINNAMEDst <$> replicateM len (get :: Get Char)
      _ -> error "not support [ipv6] yet"
    dstPort <- getWord16be >>= return . fromIntegral . toInteger
    return $ ST.RequestMsg cmd reqAtyp dstAddr dstPort

    
instance Binary (ST.ReplyMsg) where
  put ST.ReplyMsg{..} = do
    putWord8 5
    putWord8Enum rep
    putWord8 0
    putWord8Enum repAtyp
    case (repAtyp, bndAddr) of
      (ST.IPV4, ST.IPDst addr) -> putWord32be addr
      (ST.DOMAINNAME, ST.DOMAINNAMEDst addr) -> do
        putWord8 $ fromIntegral $ length addr
        mapM_ put addr
      (ST.IPV6, _) -> error "not support ipv6 yet"
      _ -> error "reply [atyp] not match with [bndAddr]"
    putWord16be $ fromIntegral bndPort
    
  get = do
    getWord8
    rep <- getWord8Enum :: Get ST.ReplyResult
    getWord8
    repAtyp <- getWord8Enum
    bndAddr <- case repAtyp of
      ST.IPV4 -> getWord32be >>= return . ST.IPDst
      ST.DOMAINNAME -> do
        len <- fromEnum <$> getWord8
        ST.DOMAINNAMEDst <$> replicateM len (get :: Get Char)
      _ -> error "not support ipv6 yet"
    bndPort <- fromEnum <$> getWord16be
    return $ ST.ReplyMsg rep repAtyp bndAddr bndPort


      -- --------------------
      -- client-to-server msg
      -- --------------------
instance Binary ST.CSMsg where
  put ST.CSMsg{..} = do
    case csTargetAddr of
      ST.CSTargetAddr (ST.IPDst addr) port -> do
        putWord8 1
        put addr
        putWord16be $ fromIntegral port
      ST.CSTargetAddr (ST.DOMAINNAMEDst addr) port -> do
        putWord8 2
        putWord8 $ fromIntegral $ length addr
        mapM_ put addr
        putWord16be $ fromIntegral port
    putWord8Enum csType

  get = do
    t <- getWord8
    addr <- case t of
      1 -> ST.IPDst <$> get
      2 -> do
        len <- fromIntegral . toInteger <$> getWord8
        ST.DOMAINNAMEDst <$> replicateM len get
    port <- fromIntegral . toInteger <$> getWord16be
    csType <- getWord8Enum
    return $ ST.CSMsg (ST.CSTargetAddr addr port) csType
    
-- aux
getWord8Enum :: Enum a => Get a
getWord8Enum = toEnum . fromIntegral . toInteger <$> getWord8

putWord8Enum :: Enum a => a -> Put
putWord8Enum = putWord8 . fromIntegral . fromEnum 
