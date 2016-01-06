{-# LANGUAGE OverloadedStrings #-}

module Negotiate (
  negotiateSock5,
  negotiateCSClient,
  negotiateCSClientTC,
  negotiateCSServer,
  ) where

import Network.Socket(Socket)
import Network.Socket.ByteString.Lazy
import Data.Binary
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TVar as TV
import Control.Monad.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Text.Printf

import Type
import Message
import qualified Socks5 as S5
import qualified Config as C
import qualified CryptoMsg as CM

       
negotiateSock5 :: Socket -> C.ServerConfig -> IO CSMsg
negotiateSock5 sock config = do
  reqMsg <- S5.negotiate sock config
  return $ CSMsg {
    csTargetAddr = CSTargetAddr (dstAddr reqMsg) (dstPort reqMsg),
    csType = cmd reqMsg}
  
negotiateCSClient :: Socket -> CSMsg -> IO ()
negotiateCSClient = send_CSMsg

negotiateCSClientTC :: TC.TChan BS.ByteString -> CSMsg -> IO ()
negotiateCSClientTC = write_CSMsg

negotiateCSServer :: Socket -> IO (CSMsg, BS.ByteString, BS.ByteString, BS.ByteString)
negotiateCSServer sock = do
  (r, decodeLeft, unconsumedPlain, unconsumedEncrypted) <- recv_CSMsg sock
  return $ (r, decodeLeft, unconsumedPlain, unconsumedEncrypted)

      -- aux
send_CSMsg :: Socket -> CSMsg -> IO ()      
send_CSMsg sock msg =
  sendAll sock (encode msg) >>= const (return ())

write_CSMsg :: TC.TChan BS.ByteString -> CSMsg -> IO ()
write_CSMsg chan msg =
  atomically $ TC.writeTChan chan $ BSL.toStrict $ encode msg
  
  --                             inputLeft,     unconsumedPlain, unconsumedEncrypted
recv_CSMsg :: Socket -> IO (CSMsg, BS.ByteString, BS.ByteString, BS.ByteString)
recv_CSMsg sock = do
  (r, unconsumedPlain, unconsumedEncrypted) <- recv sock 4096 >>=
                                              return . CM.decryptMsg "" . BSL.toStrict
  decodeRes <- return . decodeOrFail $ BSL.fromStrict r
  case decodeRes of
    Left _ -> error "illegal CSMsg"
    Right (inputLeft, _, msg) -> do
      -- if BSL.length inputLeft /= 0 then
      --   print ("inputLeft: " ++ show inputLeft ++ "\n" ++ show unconsumedPlain ++ "\n"
      --         ++ show unconsumedEncrypted ++ "\n")
      --   else return ()
      return (msg, BSL.toStrict inputLeft, unconsumedPlain, unconsumedEncrypted)
  
