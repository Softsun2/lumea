module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified System.Directory as D
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Monad (void)
import Control.Concurrent (forkFinally)

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
  packet <- recv s 128  -- get header should be <= 128 bytes
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

-- TODO: limit the number of threads to something reasonable,
-- realisitcally I won't be getting too much traffic
getOnlyServer :: Socket -> [FilePath] -> IO ()
getOnlyServer s roots = do
  (c, _) <- accept s
  -- It's important to send fin when it's unlikely the server
  -- reads the entire request.
  void $ forkFinally (onAccept c) (const $ gracefulClose c 4000)
  getOnlyServer s roots

  where
    respond :: Socket -> String -> IO ()
    respond c request = do
      resource <- D.findFile roots (parseResource request)
      case resource of
        Just fp -> do
          contents <- readFile fp
          sendAll c $ BSC.pack okHeader <> BSC.pack contents
        Nothing -> sendAll c $ BSC.pack badReqHeader
    onAccept :: Socket -> IO ()
    onAccept c = do
      getRequest <- readGet c
      case getRequest of
        Parsable request -> respond c $ BSC.unpack request
        ClientDisconnect -> return ()
        BadRequest -> respondToBadRequest c
    

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
  
