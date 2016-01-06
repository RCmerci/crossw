module Socks5  (
  negotiate,
  ) where

import Network.Socket(Socket, inet_addr)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.ByteString.Lazy as BL
import Control.Monad
import Message 
import Type

import qualified Config as C

negotiate :: Socket -> C.ServerConfig -> IO RequestMsg
negotiate sock serverConfig = do
  initMsg <- recv_InitMsg sock
  let responseInitMsg = ResponseInitMsg NO_AUTH 
  send_ResponseInitMsg sock responseInitMsg
  requestMsg <- recv_RequestMsg sock
  case cmd requestMsg of
    CONNECT -> return ()
    _ -> error "only support [CONNECT] yet"
  serverHostAddress <- inet_addr $ C.serverHost serverConfig
  let replyMsg = ReplyMsg Success IPV4 (IPDst serverHostAddress) (C.serverPort serverConfig)
  send_ReplyMsg sock replyMsg
  return requestMsg



    -- aux
recv_InitMsg :: Socket -> IO InitMsg
recv_InitMsg sock =
  recv sock 4096 >>= return . decodeOrFail >>=
  (\ r ->
    case r of
    Left _ -> error "fail in negotiateSocks5."
    Right (leftInput, _, msg) ->
      if BL.length leftInput > 0 then error "fail in negotiateSocks5." else return msg)


send_ResponseInitMsg :: Socket -> ResponseInitMsg -> IO ()
send_ResponseInitMsg sock msg = sendAll sock (encode msg) >>= const (return ())

recv_RequestMsg :: Socket -> IO RequestMsg
recv_RequestMsg sock =
  recv sock 4096 >>= return . decodeOrFail >>=
  (\ r ->
    case r of
    Left _ -> error "fail in negotiateSocks5."
    Right (leftInput, _, msg) ->
      if BL.length leftInput > 0 then error "fail in negotiateSocks5." else return msg)

send_ReplyMsg :: Socket -> ReplyMsg -> IO ()
send_ReplyMsg sock msg = sendAll sock (encode msg) >>= const (return ()) 
