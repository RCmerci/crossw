{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent.Async
import Control.Concurrent
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TVar as TV
import Control.Monad.STM
import Control.Exception(finally, try, SomeException, mask, onException, catch)
import Text.Printf
import Network.Socket(
  Socket,
  PortNumber,
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
  )
import Network.Socket.ByteString
import qualified Data.ByteString as BS

import qualified Config as C
import qualified Negotiate as N
import qualified Type as T
import qualified CryptoMsg as CM
import Common

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1                                 -- delete
  bindAddr <- getClientAddr
  bind sock bindAddr
  listen sock 5

  forever $ do
    (newSock, (SockAddrInet accPort accHost)) <- accept sock
    acceptIp <- inet_ntoa accHost
    -- printf "accept a new socket from %s:%s\n" acceptIp (show accPort)
    forkFinally (talkToSocks5Client newSock)
      (\ r -> do
          close newSock
          case r of
            Left e -> printf "exception: %s" (show e)
            Right _ -> return ()
          printf "talkToSocks5Client end.\n")


talkToSocks5Client :: Socket -> IO ()
talkToSocks5Client recvSock = do
  -- printf "talkToSocks5Client start.\n"
  csMsg <- N.negotiateSock5 recvSock C.serverConfig
  printf "csMsg: %s\n" $  show $ T.csTargetAddr csMsg
  sendSock <- connectServer
  
  recvChan <- TC.newTChanIO
  sendChan <- TC.newTChanIO
  recvServerChan <- TC.newTChanIO
  sendServerChan <- TC.newTChanIO
  
  flip finally (close sendSock) $ raceMany
    [(logicThread recvChan sendChan recvServerChan sendServerChan csMsg),
     (recvThread recvSock recvChan),
     (sendThread recvSock sendChan),
     ((try $ recvServerThread sendSock recvServerChan :: IO (Either IOError ())) >>
      cleanUp2 recvServerChan sendChan), 
     ((sendServerThread sendSock sendServerChan))]
    
  return ()



connectServer :: IO Socket
connectServer = do
  sock <- socket AF_INET Stream defaultProtocol
  onException (do
    {setSocketOption sock ReuseAddr 1; -- delete  
     addr <- getServerAddr;
     connect sock addr}) $ close sock
  return sock

-- cleanUp :: Socket -> TC.TChan BS.ByteString -> TC.TChan BS.ByteString -> Maybe Int -> IO ()
-- cleanUp sock recvServerChan sendChan (Just 4) = aux
--   where aux = do
--           dump
--           try sendLeftMsg :: IO (Either SomeException ())
--           return ()
--         dump = do
--           isEmpty <- atomically $ TC.isEmptyTChan recvServerChan
--           if isEmpty then return () else
--             (atomically $ TC.readTChan recvServerChan >>= TC.writeTChan sendChan) >>= const dump
--         sendLeftMsg = do
--           isEmpty <- atomically $ TC.isEmptyTChan recvServerChan
--           if isEmpty then return () else
--             sendLoop sock sendChan return
--             (\ _ -> do
--                 isEmpty <- atomically $ TC.isEmptyTChan sendChan
--                 if isEmpty then error "just for exit loop = =" else return ())
-- cleanUp _ _ _ (Just which) = do
--   printf "[warning] thread %d quit\n" which
--   return ()


    -- monitor recvChan and sendChan until all clear
cleanUp2 :: TC.TChan BS.ByteString -> TC.TChan BS.ByteString -> IO ()
cleanUp2 recvServerChan sendChan =
  concurrently
  (checkChanEmptySpin recvServerChan)
  (checkChanEmptySpin sendChan) >> return ()
  where checkChanEmptySpin chan = do
          isEmpty <- atomically $ TC.isEmptyTChan chan
          if isEmpty then return () else do threadDelay 50000;
                                            checkChanEmptySpin chan-- 50000 -> 50 ms


recvThread :: Socket -> TC.TChan BS.ByteString -> IO ()
recvThread sock chan = do
  addr <- getSocketName sock
  stateRecvLoop sock chan return ""
  
sendThread :: Socket -> TC.TChan BS.ByteString -> IO ()
sendThread sock chan =
  stateSendLoop sock chan return return ""

recvServerThread :: Socket -> TC.TChan BS.ByteString -> IO ()
recvServerThread sock chan =
  stateRecvLoop sock chan
  (\ msg -> do
      (unconsumedPlain, unconsumedEncrypted) <- get
      (res, unconsumedP, unconsumedE) <- return $ CM.decryptMsg unconsumedPlain (BS.append unconsumedEncrypted msg)
      put (unconsumedP, unconsumedE)
      return res)
  ("", "")

  
sendServerThread :: Socket -> TC.TChan BS.ByteString -> IO ()
sendServerThread sock chan = 
  stateSendLoop sock chan
  (\ msg -> return $ CM.encryptMsg msg)
  return ""
  
logicThread :: TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> TC.TChan BS.ByteString
            -> T.CSMsg -> IO ()
logicThread recvChan sendChan recvServerChan sendServerChan csMsg = do
  N.negotiateCSClientTC sendServerChan csMsg
  
  race
    (forever $ atomically $ TC.readTChan recvChan >>= TC.writeTChan sendServerChan)
    (forever $ atomically $ TC.readTChan recvServerChan >>= TC.writeTChan sendChan)
  return ()

-- decrypt :: TV.TVar T.Unconsumed -> TC.TChan BS.ByteString -> BS.ByteString -> STM BS.ByteString  
-- decrypt unconsumedTVar ungetChan msg = do
--   unconsumed <- TV.readTVar unconsumedTVar
--   plainMsg <- case unconsumed of
--     T.EmptyUnconsumed -> return ""
--     T.Unconsumed v -> return v
--   TV.writeTVar unconsumedTVar T.EmptyUnconsumed
--   case CM.decryptMsg plainMsg msg of
--     (r, unconsumedPlain, unconsumedEncrypted) -> do
--       if BS.length unconsumedPlain /= 0 then do
--         TV.writeTVar unconsumedTVar (T.Unconsumed unconsumedPlain)
--         TC.unGetTChan ungetChan unconsumedEncrypted
--         else return ()
--       return r

-- encrypt :: BS.ByteString -> STM BS.ByteString
-- encrypt = return . CM.encryptMsg
  
        
    -- aux
getServerAddr :: IO SockAddr
getServerAddr = do
  host <- inet_addr $ C.serverHost C.serverConfig
  return $ SockAddrInet (fromIntegral $ C.serverPort C.serverConfig) host

getClientAddr :: IO SockAddr
getClientAddr = do
  host <- inet_addr $ C.clientHost C.clientConfig
  return $ SockAddrInet (fromIntegral $ C.clientPort C.clientConfig) host

  
newChanCp :: IO (TC.TChan a, TC.TChan a)
newChanCp = do
  recvChan <- TC.newTChanIO
  sendChan <- TC.newTChanIO
  return (recvChan, sendChan)

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
