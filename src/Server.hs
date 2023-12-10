module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified System.Directory as D
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Concurrent

okHeader :: String
okHeader = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n"

badReqHeader :: String
badReqHeader = "HTTP/1.1 400 Bad Request\r\nContent-Type: text/html\r\n\r\n"

respondToBadRequest :: Socket -> IO ()
respondToBadRequest c = sendAll c $ BSC.pack badReqHeader

data ReadResult =
  Parsable BSC.ByteString |
  ClientDisconnect |
  BadRequest

readGet :: Socket -> IO ReadResult
readGet s = do
  -- Only read 4096 bytes. If this causes a short read the client's
  -- connection will reset when sending the response.
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

parseResource :: String -> FilePath
parseResource str = tail $ words str !! 1

getOnlyServer :: Socket -> [FilePath] -> IO ()
getOnlyServer s roots = do
  (c, _) <- accept s
  getRequest <- readGet c
  case getRequest of
    Parsable request -> respondToGet c $ BSC.unpack request
    ClientDisconnect -> return ()
    BadRequest -> respondToBadRequest c
  close c
  getOnlyServer s roots
  where
    respondToGet :: Socket -> String -> IO ()
    respondToGet c request = do
      resource <- D.findFile roots (parseResource request)
      case resource of
        Just fp -> do
          contents <- readFile fp
          sendAll c $ BSC.pack okHeader <> BSC.pack contents
        Nothing -> sendAll c $ BSC.pack badReqHeader

run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol

  let addr = tupleToHostAddress (127, 0, 0, 1)
  let port = 8081
  let maxListenQueueLength = 128
  
  bind s $ SockAddrInet port addr
  listen s maxListenQueueLength
  getOnlyServer s ["/Users/softsun2/softsun2/dev/personal/lumea/site/html"]

  close s
