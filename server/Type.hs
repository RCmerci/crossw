module Type (
  -- socks5
  Method(..),
  Methods(..),
  Nmethods(..),
  InitMsg(..),
  ResponseInitMsg(..),
  Cmd(..),
  AddrType(..),
  DstPort(..),
  DstAddr(..),
  RequestMsg(..),
  ReplyResult(..),
  BndPort(..),
  BndAddr(..),
  ReplyMsg(..),
  -- client-to-server
  CSTargetAddr(..),
  CSType(..),
  CSMsg(..),
  -- encrypt type
  
  -- ------------
  Unconsumed(..),
  ) where

import qualified Network.Socket as NS (HostAddress)
import qualified Data.ByteString as BS


       -- ---------------------
       -- socks5
       -- ---------------------
data Method = NO_AUTH | USERNAME_PASSWORD deriving (Show)
type Methods = [Method]
type Nmethods = Int
data Cmd = CONNECT | BIND | UDP_ASSOCIATE deriving (Show)
data AddrType = IPV4 | DOMAINNAME | IPV6 deriving (Show)
type DstPort = Int             
data DstAddr = IPDst NS.HostAddress | DOMAINNAMEDst String deriving (Show)
data ReplyResult =
  Success |
  Fail                          -- add it later
  deriving (Show)
type BndPort = DstPort
type BndAddr = DstAddr


data InitMsg = InitMsg Nmethods Methods deriving (Show)
data ResponseInitMsg = ResponseInitMsg Method deriving (Show)
            
data RequestMsg = RequestMsg {
  cmd :: Cmd,
  reqAtyp :: AddrType,
  dstAddr :: DstAddr,
  dstPort :: DstPort -- network order
  } deriving (Show)


data ReplyMsg = ReplyMsg {
  rep :: ReplyResult,
  repAtyp :: AddrType,
  bndAddr :: BndAddr,
  bndPort :: BndPort -- network order
  } deriving (Show)

             -- --------------------
             -- client-to-server msg
             -- --------------------
data CSTargetAddr = CSTargetAddr DstAddr DstPort
type CSType = Cmd
data CSMsg = CSMsg {
  csTargetAddr :: CSTargetAddr,
  csType :: CSType
  } deriving (Show)




             -- --------------------
             -- encrypt msg  
             -- --------------------
             -- 加密的格式只要加密模块自己内部知道就行了，
             -- 外部只要加密数据或解密数据就行了，
             -- 所以这个type写在这个type.hs 没有什么意义，encrypt模块内部用就行了


             -- --------------------
data Unconsumed = Unconsumed BS.ByteString | EmptyUnconsumed deriving (Show)

             
             -- --------------
             -- enum
             -- --------------
instance Enum Method where
  toEnum 0 = NO_AUTH
  toEnum 2 = USERNAME_PASSWORD
  toEnum _ = error "socks5 METHOD not supported."
  
  fromEnum NO_AUTH = 0
  fromEnum USERNAME_PASSWORD = 2


instance Enum Cmd where
  toEnum 1 = CONNECT
  toEnum 2 = BIND
  toEnum 3 = UDP_ASSOCIATE
  toEnum _ = error "illegal socks5 CMD."

  fromEnum CONNECT = 1
  fromEnum BIND    = 2
  fromEnum UDP_ASSOCIATE = 3

instance Enum AddrType where
  toEnum 1 = IPV4
  toEnum 3 = DOMAINNAME
  toEnum 4 = IPV6
  toEnum _ = error "illegal socks5 AddrType."

  fromEnum IPV4 = 1
  fromEnum DOMAINNAME = 3
  fromEnum IPV6 = 4

instance Enum ReplyResult where
  toEnum 0 = Success
  toEnum _ = Fail

  fromEnum Success = 0
  fromEnum Fail = 1

                  -- show

instance Show CSTargetAddr where
  show (CSTargetAddr (IPDst addr) port) =
    concat [show addr, ":", show port]
  show (CSTargetAddr (DOMAINNAMEDst addr) port) =
    concat [addr, ":", show port]
