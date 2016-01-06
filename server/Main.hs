{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception(finally, try, SomeException, mask, onException, throwIO, catch)
import Network.Socket(
  Socket,
  PortNumber,
  HostAddress,
  socket,
  Family(..),
  SocketType(..),
  defaultProtocol,
  listen,
  bind,
  accept,
  connect,
  SockAddr(..),
  inet_addr,
  inet_ntoa,
  close,
  setSocketOption,
  SocketOption(..),
  getSocketName,
  getPeerName,
  )
import Network.BSD(
  getHostByName,
  HostEntry(..),
  )
import Network.Socket.ByteString
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TVar as TV
import qualified Data.ByteString as BS
import Text.Printf

import qualified Negotiate as N
import qualified Config as C
import qualified Type as T
import qualified CryptoMsg as CM
import Common



main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1                              -- delete
  bindAddr <- getServerAddr
  bind sock bindAddr
  listen sock 5

  forever $ do
    (newSock, (SockAddrInet accPort accHost)) <- accept sock
    acceptIp <- inet_ntoa accHost
    -- printf "accept a new socket from %s:%s\n" acceptIp (show accPort)    
    forkFinally (workThread newSock)
      (\ r -> do
          close newSock
          case r of
            Left e -> printf "exception: %s" (show e)
            Right _ -> return ()
          printf "workThread end\n"
          return ())

workThread :: Socket -> IO ()
workThread sock = do
  recvChan <- TC.newTChanIO
  sendChan <- TC.newTChanIO
  sendRemoteChan <- TC.newTChanIO
  recvRemoteChan <- TC.newTChanIO
  (csMsg, decodeLeft, unconsumedPlain, unconsumedEncrypted) <- N.negotiateCSServer sock
  atomically $ TC.unGetTChan recvChan decodeLeft
  -- printf "recv csMsg: %s\n" $ show csMsg
  targetSock <- connectTarget csMsg
  flip finally (close targetSock) $ raceMany
    [(logicThread recvChan sendChan recvRemoteChan sendRemoteChan csMsg),
     (recvClientThread sock recvChan (unconsumedPlain, unconsumedEncrypted)),
     (sendClientThread sock sendChan),
     (sendRemoteThread targetSock sendRemoteChan),
     ((try $ recvRemoteThread targetSock recvRemoteChan :: IO (Either IOError ())) >>
      cleanUp2 recvRemoteChan sendChan)]
    
  return ()



connectTarget :: T.CSMsg -> IO Socket
connectTarget msg = do
  sock <- socket AF_INET Stream defaultProtocol
  case T.csTargetAddr msg of
    T.CSTargetAddr (T.IPDst addr) port ->
      connect sock $ SockAddrInet (fromIntegral port) addr
    T.CSTargetAddr (T.DOMAINNAMEDst hostName) port -> do
      hostAddr <- getHostByName hostName
      case hostAddresses hostAddr of
        [] -> error "[getHostByName] fail"
        l@(_:_) -> connectConcurrently sock l port

  local <- getSocketName sock
  _remote <- getPeerName sock
  printf "%s -> %s\n" (show local) (show $ T.csTargetAddr msg)
  return sock



connectConcurrently :: Socket -> [HostAddress] -> Int -> IO ()
connectConcurrently sock hosts port = 
  raceListTimeout 10000000
  (\ host -> do
      connect sock $ SockAddrInet (fromIntegral port) host)
  hosts
  >>= (\ r -> case r of
        Nothing -> do
          close sock
          hostsStr <- fmap join $ sequence $ fmap (\h -> inet_ntoa h >>= return . (++ "|") ) hosts
          error ("connect fail: " ++ hostsStr)
        Just _ -> return ())
  


cleanUp2 :: TC.TChan BS.ByteString -> TC.TChan BS.ByteString -> IO ()
cleanUp2 recvRemoteChan sendChan =
  concurrently
  (checkChanEmptySpin recvRemoteChan)
  (checkChanEmptySpin sendChan) >> return ()
  where checkChanEmptySpin chan = do
          isEmpty <- atomically $ TC.isEmptyTChan chan
          if isEmpty then return () else do threadDelay 50000;
                                            checkChanEmptySpin chan-- 50000 -> 50 ms

    
recvClientThread :: Socket -> TC.TChan BS.ByteString -> (BS.ByteString, BS.ByteString) -> IO ()
recvClientThread sock chan initState =
  stateRecvLoop sock chan
  (\ msg -> do
      (unconsumedPlain, unconsumedEncrypted) <- get
      (res, unconsumedP, unconsumedE) <- return $ CM.decryptMsg unconsumedPlain (BS.append unconsumedEncrypted msg)
      put (unconsumedP, unconsumedE)
      return res)
  initState
                             
recvRemoteThread :: Socket -> TC.TChan BS.ByteString -> IO ()
recvRemoteThread sock chan =
  stateRecvLoop sock chan return ""

            
sendClientThread :: Socket -> TC.TChan BS.ByteString -> IO ()
sendClientThread sock chan =
  stateSendLoop sock chan
  (\ msg -> return $ CM.encryptMsg msg)
  return ""
              
sendRemoteThread :: Socket -> TC.TChan BS.ByteString -> IO ()
sendRemoteThread sock chan = do
  addr <- getSocketName sock
  stateSendLoop sock chan return return ""

  
logicThread :: TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> T.CSMsg -> IO ()
logicThread recvChan sendChan recvRemoteChan sendRemoteChan _msg = do
  
  race
    (forever $ atomically $ TC.readTChan recvChan >>= TC.writeTChan sendRemoteChan)
    (forever $ atomically $ TC.readTChan recvRemoteChan >>= TC.writeTChan sendChan)
  return ()

    -- aux

getServerAddr :: IO SockAddr
getServerAddr = do
  host <- inet_addr $ C.serverHost C.serverConfig
  return $ SockAddrInet (fromIntegral $ C.serverPort C.serverConfig) host
  


type BeforeSend a s = a -> StateT s IO BS.ByteString
type AfterSend s a1 = BS.ByteString -> StateT s IO a1

stateSendLoop
  ::  Socket
     -> TC.TChan a
     -> BeforeSend a s
     -> AfterSend s a1
     -> s
     -> IO ()
stateSendLoop sock chan before after initState =
  (mask $ \ restore -> flip runStateT initState $ loop restore) >>
  return ()
  where loop restore = do
          msg <- liftIO $ atomically $ TC.readTChan chan
          fixMsg <- before msg
          liftIO $ restore (sendAll sock fixMsg)
            `onException` (atomically $ TC.unGetTChan chan msg)
          after fixMsg
          loop restore
            
    

type BeforeWriteChan s a = BS.ByteString -> StateT s IO a

stateRecvLoop
  :: Socket
    -> TC.TChan a -> BeforeWriteChan s a -> s -> IO ()
stateRecvLoop sock chan beforeWriteToChan initState =
  (mask $ \ restore -> flip runStateT initState $ loop restore) >>
  return ()
  where loop restore = do
          msg <- liftIO $ recv sock 4096
          if BS.length msg == 0 then return () else do
            fixMsg <- beforeWriteToChan msg
            liftIO $ restore (atomically $
                              TC.writeTChan chan fixMsg)
              `onException`
              (atomically $ TC.writeTChan chan fixMsg)
            loop restore
