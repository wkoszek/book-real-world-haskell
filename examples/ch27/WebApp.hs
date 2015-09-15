{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleContexts, FlexibleInstances, OverlappingInstances #-}
module WebApp
    (
      App
    , runApp
    , HttpError(..)
    , Handler
    , HttpRequest(..)
    , HttpResponse(..)
    , HttpConnection(..)
    , ok
    , httpError
    , dispatch
    , serverLoop
    , url
    , respStatus
    ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Monad.State
import qualified System.IO
import Network (PortID(..), accept, listenOn, sClose, withSocketsDo)
import Control.Exception (bracket, finally)
import System.IO (Handle)

import JSONClass
import MonadHandle
import HttpParser
import URLParser


ok :: (JSON a, Monad m) => a -> m JValue
ok = return . toJValue

httpError :: (JSON a, MonadError HttpResponse m) => HttpError -> a -> m b
httpError err = throwError . RespError err [] . toJValue

type Handler s a = HttpRequest -> App s a

data HttpError = BadRequest
               | NotFound
               | InternalServerError
                 deriving (Show)

instance Error HttpResponse where
    strMsg = RespError InternalServerError [] . JString

data HttpResponse =
    RespSuccess {
      respHeaders :: [String]
    , respBody :: JValue
    } |
    RespError {
      respError_ :: HttpError
    , respHeaders :: [String]
    , respBody :: JValue
    } deriving (Show)

respStatus :: HttpResponse -> String
respStatus (RespSuccess _ _) = "200 OK"
respStatus (RespError BadRequest _ _) = "400 Bad Request"
respStatus (RespError NotFound _ _) = "404 Not Found"
respStatus (RespError InternalServerError _ _) = "500 Internal Server Error"

newtype App s a = App (ErrorT HttpResponse (ReaderT HttpConnection (StateT s IO)) a)
    deriving (Functor, Monad, MonadIO, MonadError HttpResponse,
              MonadReader HttpConnection, MonadState s)

runApp :: s -> HttpConnection -> App s a -> IO (Either HttpResponse a, s)
runApp st req (App a) = runStateT (runReaderT (runErrorT a) req) st

serverLoop :: s -> App s () -> Int -> IO ()
serverLoop st serv port = liftIO . withSocketsDo $ do
  let port' = PortNumber (fromIntegral port)
  bracket (listenOn port') sClose $ \sock -> do
    putStrLn $ "listening on port " ++ show port
    forever $ do
      (handle, clientHost, clientPort) <- accept sock
      putStrLn $ "connect from " ++ show (clientHost, clientPort)
      let req = HttpConnection { connClient = show clientHost
                               , connHandle = handle }
      forkIO $ finally (runApp st req serv >> return ()) (hClose handle)

data HttpConnection = HttpConnection {
      connClient :: String
    , connHandle :: Handle
    }

url :: (Method -> Bool) -> URLParser (Handler s JValue) -> HttpRequest
    -> Maybe (Handler s JValue)
url methOK p req = do
  guard . methOK $ httpMethod req
  either (const Nothing) Just $ parse p "" (httpURL req)

dispatch :: [HttpRequest -> Maybe (Handler s JValue)] -> HttpRequest
         -> App s JValue
dispatch hs req = do
  case map ($req) hs of
    (Just f:_) -> f req
    _ -> httpError NotFound "not found"

instance MonadIO m => MonadHandle System.IO.Handle m where
    openFile path mode = liftIO $ System.IO.openFile path mode
    hPutStr h s = liftIO $ System.IO.hPutStr h s
    hClose = liftIO . System.IO.hClose
    hGetContents = liftIO . System.IO.hGetContents
    hPutStrLn h s = liftIO $ do
                      System.IO.hPutStr h s
                      System.IO.hPutStr h "\r\n"
