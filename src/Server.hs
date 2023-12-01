module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Concurrent

dummyOkResponse :: String
dummyOkResponse = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, World!\r\n"

dummyBadResponse :: String
dummyBadResponse = "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\n400 Bad Request: Your request is invalid.\r\n"

respondToGet :: Socket -> String -> IO ()
respondToGet c _ = sendAll c $ BSC.pack dummyOkResponse

respondToBadRequest :: Socket -> IO ()
respondToBadRequest c = sendAll c $ BSC.pack dummyBadResponse

data ReadResult =
  Parsable BSC.ByteString |
  ClientDisconnect |
  BadRequest deriving Show

readGet :: Socket -> IO ReadResult
readGet s = do
  -- Only read 4096 bytes. If this causes a short read the client's
  -- connection will reset when sending the response. Maybe they
  -- should send a reasonably sized request...
  packet <- recv s 4096
  let (getHeader, _) = BS.breakSubstring (BSC.pack "\r\n") packet
  let parsableGetHeader =
        BSC.pack "GET" `BS.isPrefixOf` getHeader &&
        length (BS.split 32 getHeader) == 3
    in
    case (BS.null getHeader, parsableGetHeader) of
      (False, True) -> return $ Parsable getHeader
      (True, _) -> return ClientDisconnect
      (_, False) -> return BadRequest
    
getOnlyServer :: Socket -> IO ()
getOnlyServer s = do
  (c, _) <- accept s
  getRequest <- readGet c
  case getRequest of
    Parsable request -> respondToGet c $ BSC.unpack request
    ClientDisconnect -> return ()
    BadRequest -> respondToBadRequest c
  close c
  getOnlyServer s

run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol

  let addr = tupleToHostAddress (127, 0, 0, 1)
  let port = 8081
  let maxListenQueueLength = 128
  
  bind s $ SockAddrInet port addr
  listen s maxListenQueueLength
  getOnlyServer s

  close s
