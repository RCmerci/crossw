module Config (
  ServerConfig(..),
  serverConfig,
  ClientConfig(..),
  clientConfig,
  password,
  ) where

import Network.Socket

data ServerConfig = ServerConfig {
  serverHost :: String,
  serverPort :: Int
  }
data ClientConfig = ClientConfig {
  clientHost :: String,
  clientPort :: Int
  }


serverConfig :: ServerConfig
serverConfig = ServerConfig "1.1.1.1" 23334

clientConfig :: ClientConfig
clientConfig = ClientConfig "127.0.0.1" 23333

password = "xxxxxxxx"
