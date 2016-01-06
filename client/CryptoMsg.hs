{-# LANGUAGE OverloadedStrings #-}

module CryptoMsg (
  encryptMsg,
  decryptMsg,
  ) where

import Crypto.Cipher.Types
import Crypto.Cipher
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Config as C

passwdTo32bytes :: String -> ByteString
passwdTo32bytes s =
  BS8.pack $ Prelude.take 32 $ s ++ Prelude.replicate 32 '0'

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
ctx :: AES256
ctx = either (error . show) cipherInit $ makeKey $ passwdTo32bytes C.password
      --                      
encryptMsg :: ByteString -> ByteString
encryptMsg msg =
  ecbEncrypt ctx $ BSL.toStrict $ paddingMsg msg

             --                          decryptedStr , unconsumed
decryptMsg16 :: ByteString -> ByteString -> (ByteString, ByteString)
decryptMsg16 plainMsg encryptedMsg =
  (BS8.concat $ fmap (\ (EncryptMsg _ _ r) -> r) encryptMsgL,
   BSL.toStrict unconsumed)
  where msg = ecbDecrypt ctx encryptedMsg
        (encryptMsgL, unconsumed) = decodeUntilFail $ BSL.append (BSL.fromStrict plainMsg) (BSL.fromStrict msg)
             --                          decryptedStr , unconsumed, unconsumedEncrypted
decryptMsg :: ByteString -> ByteString -> (ByteString, ByteString, ByteString)
decryptMsg plainMsg encryptedMsg =
  case (decryptMsg16 plainMsg msg16) of
  (d, u) -> (d, u, unconsumedEncryptedMsg)
  where msgLen = BS8.length encryptedMsg
        mod16 = msgLen `mod` 16
        msg16 = if mod16 /= 0 then BS8.take (msgLen - mod16) encryptedMsg else encryptedMsg
        unconsumedEncryptedMsg = if mod16 /= 0 then BS8.drop (msgLen - mod16) encryptedMsg
                                 else BS8.empty

decodeUntilFail :: BSL.ByteString -> ([EncryptMsg], BSL.ByteString)
decodeUntilFail = aux []
  where aux res msg =
          case decodeOrFail msg of
          Left _ -> (Prelude.reverse res, msg)
          Right (leftMsg, _, a) -> aux (a:res) leftMsg
  
             -- 加密需要padding 来符合 16倍数的字节的要求 --
paddingMsg :: ByteString -> BSL.ByteString
paddingMsg msg =
  encode (EncryptMsg paddingLen dataLen msg)
  where m = (BS8.length msg + 5) `mod` 16       --  5 -> paddinglen + datalen=5byte
        paddingLen = case m of
          0 -> 0
          len -> 16 - len
        dataLen = BS8.length msg

-- +--------------+------------+---------------------+------------+
-- |  paddinglen  |   datalen  |  padding            |  data      |
-- +--------------+------------+---------------------+------------+ 

             -- paddinglen : 1 byte （0-16） padding长度
             -- datalen : 4 byte , data 长度
             -- data : 被加密数据
             -- padding : 一堆 '0'

data EncryptMsg = EncryptMsg PaddingLen DataLen Data deriving (Show)
type DataLen = Int
type PaddingLen = Int
type Data = ByteString

instance Binary EncryptMsg where
  put (EncryptMsg pl dl d) = do
    putWord8 $ fromIntegral pl
    putWord32be $ fromIntegral dl
    putByteString $ BS8.replicate pl '0'
    putByteString d
    
  get = do
    pl <- getWord8 >>=  return . fromEnum . toInteger
    dl <- getWord32be >>=  return . fromEnum . toInteger
    getByteString pl
    d <- getByteString dl
    return (EncryptMsg pl dl d)




