module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Concurrent

dummyResponse :: String
dummyResponse = "HTTP/1.1 200 OK\nContent-Type: text/plain\n\nHello, World!\n"

openFile :: FilePath -> Maybe String
openFile filePath = Just dummyResponse

isGetRequest :: String -> Bool
isGetRequest request = take 3 (head $ lines request) == "GET"

respondToGet :: Socket -> String -> IO ()
respondToGet c request = do
  sendAll c $ BSC.pack dummyResponse

lumeaRead :: Socket -> Int -> IO BSC.ByteString
lumeaRead s limit =
  let reading :: BSC.ByteString -> IO BSC.ByteString
      reading bytes = do
        packet <- recv s 256
        let numRead = BS.length $ bytes <> packet
        case (BS.null packet || BS.length packet == 0, numRead <= limit) of
          (True, True) -> return $ bytes <> packet
          (False, True) -> reading $ bytes <> packet
          _ -> error $ "Request of size " <> show numRead <> " too large."
  in reading $ BSC.pack ""


getOnlyServer :: Socket -> IO ()
getOnlyServer s = do
  (c, _) <- accept s
  request <- recv c $ 2^10
  putStrLn $ BSC.unpack request
  respondToGet c $ BSC.unpack request
  close c
  getOnlyServer s

run :: IO ()
run = do
  s <- socket AF_INET Stream defaultProtocol

  let addr = tupleToHostAddress (127, 0, 0, 1)
  let port = 8081
  let maxConnections = 1
  
  bind s $ SockAddrInet port addr
  listen s maxConnections
  getOnlyServer s

  close s
