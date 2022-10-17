module Lib (someFunc) where

import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.UTF8 (fromString)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
  ( Request,
    defaultRequest,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    setRequestHost,
    setRequestManager,
    setRequestPath,
    setRequestPort,
    setRequestSecure,
  )

someFunc :: IO ()
someFunc = do
  let urls =
        [ urlDefaultPath "perdu.com",
          urlDefaultPath "google.fr",
          URL "httpstat.us" "500"
        ]
  printer <- newPrinter
  mapM_ (printPage printer) urls

type Host = String

type Path = String

data URL = URL Host Path

urlDefaultPath :: String -> URL
urlDefaultPath host = URL host "/"

newtype Printer = Printer Manager

newPrinter :: IO Printer
newPrinter = do
  manager <- newManager tlsManagerSettings
  return $ Printer manager

printPage :: Printer -> URL -> IO ()
printPage (Printer manager) url = do
  page <- getPage manager url
  maybe mempty putStrLn page

getPage :: Manager -> URL -> IO (Maybe [Char])
getPage manager url = do
  let request = getRequest url manager
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> return . Just . toString . getResponseBody $ response
    _ -> return Nothing

getRequest :: URL -> Manager -> Request
getRequest (URL host path) manager =
  setRequestManager manager $
    setRequestSecure True $
      setRequestPort 443 $
        setRequestPath (fromString path) $
          setRequestHost (fromString host) defaultRequest
