> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTSyntax #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE TypeApplications #-}
> module JSONRPC where

> import Control.Applicative
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Control.Exception
> import Control.Lens hiding ((.=))
> import Control.Monad.IO.Class
> import qualified Data.Aeson as JSON
> import Data.Aeson ((.:), (.:!), (.=))
> import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
> import Data.Binary.Builder
> import qualified Data.ByteString.Lazy as BS
> import qualified Data.ByteString as SBS
> import Data.Map (Map)
> import qualified Data.Map as M
> import Data.Maybe (maybeToList)
> import Data.Scientific (Scientific)
> import Data.Text (Text)
> import qualified Data.Text as T
> import GHC.Stack
> import Network.Wai (strictRequestBody)
> import System.IO
> import Web.Scotty

> import Debug.Trace

> import Netstrings

We only support JSON-RPC 2.0.

> jsonRPCVersion :: Text
> jsonRPCVersion = "2.0"


A server can receive requests or notifications, and must reply to them.

A server has _state_, which is stored in an MVar to allow easy
concurrency, a collection of methods, each of which is a function
from the JSON value that is the parameter to a JSON value that is the
response.

Methods come in three forms:
 - commands, which can modify state and return an answer to the client;
 - queries, which return an answer but do not modify state; and
 - notifications, which can modify state but do not return an answer.

> data Method s where
>   Command      :: (RequestID -> s -> JSON.Value -> IO (s, JSON.Value)) -> Method s
>   Query        :: (RequestID -> s -> JSON.Value -> IO JSON.Value)      -> Method s
>   Notification :: (             s -> JSON.Value -> IO s)               -> Method s

An application is a state and a mapping from names to methods.

> data App s =
>   App { _appState :: MVar s
>       , _appMethods :: Map Text (Method s)
>       }
>
> appState :: Simple Lens (App s) (MVar s)
> appState = lens _appState (\a s -> a { _appState = s })
>
> appMethods :: Simple Lens (App s) (Map Text (Method s))
> appMethods = lens _appMethods (\a s -> a { _appMethods = s })

> mkApp :: s -> [(Text, Method s)] -> IO (App s)
> mkApp initState methods =
>   App <$> newMVar initState <*> pure (M.fromList methods)

JSON RPC exceptions should be thrown by method implementations when
they want to return an error.

> data JSONRPCException =
>   JSONRPCException { errorCode :: Integer
>                    , message :: Text
>                    , errorData :: Maybe JSON.Value
>                    , errorID :: Maybe RequestID
>                    }
>   deriving Show

> instance Exception JSONRPCException

> instance JSON.ToJSON JSONRPCException where
>   toJSON exn =
>     JSON.object
>       [ "jsonrpc" .= jsonRPCVersion
>       , "id" .=
>         case errorID exn of
>           Nothing -> JSON.Null
>           Just theID -> JSON.toJSON theID
>       , "error" .=
>         (JSON.object $ ["code" .= JSON.Number (fromInteger (errorCode exn))
>                        , "message" .= JSON.String (message exn)] ++
>                        (("data" .=) <$> maybeToList (errorData exn)))
>       ]


The spec defines some errors and reserves some error codes (from
-32768 to -32000) for its own use.

> parseError :: Text -> JSONRPCException
> parseError msg =
>   JSONRPCException { errorCode = -32700
>                    , message   = "Parse error"
>                    , errorData = Just (JSON.String msg)
>                    , errorID   = Nothing
>                    }

> methodNotFound :: JSON.ToJSON a => Maybe RequestID -> Maybe a -> JSONRPCException
> methodNotFound theID meth =
>   JSONRPCException { errorCode = -32601
>                    , message   = "Method not found"
>                    , errorData = JSON.toJSON <$> meth
>                    , errorID   = theID
>                    }
> invalidRequest :: JSONRPCException
> invalidRequest =
>   JSONRPCException { errorCode = -32600
>                    , message   = "Invalid Request"
>                    , errorData = Nothing
>                    , errorID   = Nothing
>                    }


A JSON-RPC request ID is:

    An identifier established by the Client that MUST contain a
    String, Number, or NULL value if included. If it is not included
    it is assumed to be a notification. The value SHOULD normally not
    be Null [1] and Numbers SHOULD NOT contain fractional parts

> data RequestID = IDText !Text | IDNum !Scientific | IDNull
>   deriving (Eq, Ord, Show)

> instance JSON.FromJSON RequestID where
>   parseJSON (JSON.String str) = pure (IDText str)
>   parseJSON (JSON.Number i)   = pure (IDNum i)
>   parseJSON JSON.Null         = pure IDNull
>   parseJSON other             = JSON.typeMismatch "Request ID" other
>
> instance JSON.ToJSON RequestID where
>   toJSON (IDText str) = JSON.String str
>   toJSON (IDNum i)    = JSON.Number i
>   toJSON IDNull       = JSON.Null


Because the presence of null and the absence of the key are distinct,
we have both a null constructor and wrap it in a Maybe. When the
request ID is not present, the request is a notification and the field
is Nothing.

> data Request =
>   Request { _requestMethod :: !Text
>           , _requestID :: !(Maybe RequestID)
>           , _requestParams :: !JSON.Value
>           }
>   deriving (Show)

> requestMethod :: Simple Lens Request Text
> requestMethod = lens _requestMethod (\r m -> r { _requestMethod = m })

> requestID :: Simple Lens Request (Maybe RequestID)
> requestID = lens _requestID (\r i -> r { _requestID = i })

> requestParams :: Simple Lens Request JSON.Value
> requestParams = lens _requestParams (\r m -> r { _requestParams = m })

> suchThat :: HasCallStack => JSON.Parser a -> (a -> Bool) -> JSON.Parser a
> suchThat parser pred =
>   do res <- parser
>      if pred res
>        then return res
>        else fail "invalid value"

> instance JSON.FromJSON Request where
>   parseJSON =
>     JSON.withObject ("JSON-RPC " <> T.unpack jsonRPCVersion <> " request") $
>     \o ->
>       (o .: "jsonrpc" `suchThat` (== jsonRPCVersion)) *>
>       (Request <$> o .: "method" <*> o .:! "id" <*> o .: "params")


> handleRequest :: forall s . MVar (BS.ByteString -> IO ()) -> App s -> Request -> IO ()
> handleRequest outH app req =
>   let method   = view requestMethod req
>       params   = view requestParams req
>       reqID    = view requestID req
>       theState = view appState app
>   in
>     case M.lookup method $ view appMethods app of
>       Nothing -> throw $ methodNotFound reqID (Just method)
>       Just m ->
>         case m of
>           Command impl ->
>             do rid <- requireID reqID
>                answer <- modifyMVar theState $ \s -> impl rid s params
>                let response = JSON.object [ "jsonrpc" .= jsonRPCVersion
>                                           , "id" .= reqID
>                                           , "result" .= answer
>                                           ]
>                withMVar outH $ \h -> h (JSON.encode response)
>           Query impl ->
>             do rid <- requireID reqID
>                answer <- readMVar theState >>= \s -> impl rid s params
>                let response = JSON.object [ "jsonrpc" .= jsonRPCVersion
>                                           , "id" .= reqID
>                                           , "result" .= answer
>                                           ]
>                withMVar outH $ \h -> h (JSON.encode response)
>           Notification impl ->
>             do requireNoID reqID
>                modifyMVar theState $
>                 \s -> do s' <- impl s params
>                          return (s', ())
>
>   where
>     requireID (Just rid) = return rid
>     requireID Nothing  = throwIO invalidRequest
>
>     requireNoID (Just _) = throwIO invalidRequest
>     requireNoID Nothing = return ()

One way to run a server is on stdio, listening for requests on stdin
and replying on stdout. In this system, each request must be on a
line for itself, and no newlines are otherwise allowed.

> serveStdIO :: App s -> IO ()
> serveStdIO app = init >>= loop
>   where
>     newline = 0x0a -- ASCII/UTF8
>     init = (,) <$> newMVar (BS.hPut stdout) <*> (BS.split newline <$> BS.hGetContents stdin)
>     loop (output, input) =
>       case input of
>         [] -> return ()
>         (l:rest) ->
>           do forkIO $
>                (case JSON.eitherDecode l of
>                   Left msg -> throw (parseError (T.pack msg))
>                   Right req -> handleRequest output app req)
>                `catch` reportError output
>              loop (output, rest)
>     reportError :: MVar (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
>     reportError output exn =
>       withMVar output ($ (JSON.encode exn <> BS.singleton newline))


> serveStdIONS :: App s -> IO ()
> serveStdIONS app =
>   do hSetBinaryMode stdin True
>      hSetBuffering stdin NoBuffering
>      input <- newMVar stdin
>      output <- newMVar (BS.hPut stdout . toNetstring)
>      loop output input
>   where
>     loop :: MVar (BS.ByteString -> IO ()) -> MVar Handle -> IO ()
>     loop output input =
>       do line <- withMVar input $ netstringFromHandle
>          forkIO $
>                (case JSON.eitherDecode line of
>                   Left msg -> throw (parseError (T.pack msg))
>                   Right req -> handleRequest output app req)
>                  `catch` reportError output
>          loop output input
>     reportError :: MVar (BS.ByteString -> IO ()) -> JSONRPCException -> IO ()
>     reportError output exn =
>       withMVar output ($ (JSON.encode exn))


Another way is on a socket with netstrings




Finally, HTTP also works.



> serveHTTP app port =
>     scotty port $ post "/:whatevs" $
>     do req <- request
>        body <- liftIO $ strictRequestBody req
>        -- NOTE: Making the assumption that WAI forks a thread - TODO: verify this
>        stream $ \put flush ->
>          do output <- newMVar (\ str -> put (fromByteString (BS.toStrict str)) *> flush)
>             let reportError = \ (exn :: JSONRPCException) ->
>                                 withMVar output ($ (JSON.encode exn <> BS.singleton newline))
>             (case JSON.eitherDecode body of
>                Left msg -> throw (parseError (T.pack msg))
>                Right req -> handleRequest output app req)
>              `catch` reportError
>  where newline = 0x0a -- ASCII/UTF8
