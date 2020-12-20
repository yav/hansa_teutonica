module Main(main) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IORef(IORef,newIORef,readIORef,atomicModifyIORef')
import Control.Exception(catch)

import qualified Data.Aeson as JS
import qualified Snap.Http.Server as Snap
import Network.WebSockets(Connection,sendTextData,receiveData)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS

import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Random.TF(newTFGen)
import System.FastLogger(Logger,logMsg,newLogger)

import Basics
import Game(GameState,initialGameState)
import Interact

main :: IO ()
main =
  do args <- getArgs
     rng  <- newTFGen
     srv  <- case initialGameState rng args of
               Just s -> newServer s
               Nothing ->
                 do putStrLn "Failed to create initial state."
                    exitFailure
     Snap.quickHttpServe $ WS.runWebSocketsSnap \pending ->
       do conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) (newClient srv conn)



--------------------------------------------------------------------------------

data Server = Server
  { serverRef    :: IORef State
  , serverLogger :: Logger
  }

data State = State
  { connected :: Map PlayerColor Connection
  , gameState :: InteractState
  }

newServer :: GameState -> IO Server
newServer s =
  do ref <- newIORef State { connected = Map.empty, gameState = startState s }
     logger <- newLogger "-"
     pure Server { serverRef = ref, serverLogger = logger }

serverLog :: Server -> String -> IO ()
serverLog server = logMsg (serverLogger server) . BS8.pack

serverUpate :: Server -> (State -> (State,a)) -> IO a
serverUpate srv = atomicModifyIORef' (serverRef srv)

serverUpate_ :: Server -> (State -> State) -> IO ()
serverUpate_ srv f = serverUpate srv \s -> (f s, ())

serverState :: Server -> IO State
serverState srv = readIORef (serverRef srv)

--------------------------------------------------------------------------------


newClient :: Server -> Connection -> IO ()
newClient srv conn =
  do serverLog srv "New connection requested, waiting for ID"
     mb <- recvFromMaybe conn
     case mb of
       Just color ->
         do ok <- serverUpate srv \state ->
                    if Map.member color (connected state)
                      then ( state, False)
                      else ( state { connected =
                                      Map.insert color conn (connected state) }
                           , True
                           )
            if ok then do serverLog srv ("Player connected: " ++ show color)
                          clientLoop srv color conn
                  else do serverLog srv
                                ("Player already connected: " ++ show color)
                          askDisconnect srv conn
       _ -> askDisconnect srv conn


clientLoop :: Server -> PlayerColor -> Connection -> IO ()
clientLoop srv who conn =
  loop `catch` \ex -> (ex :: WS.ConnectionException) `seq` removeClient srv who
  where
  loop =
    do mb <- recvFromMaybe conn
       case mb of
         Nothing  -> askDisconnect srv conn
         Just msg -> inMsg srv (who :-> msg) >> loop

removeClient :: Server -> PlayerColor -> IO ()
removeClient srv who =
  do serverLog srv ("Removing client: " ++ show who)
     serverUpate_ srv \state ->
                       state { connected = Map.delete who (connected state) }

askDisconnect :: Server -> Connection -> IO ()
askDisconnect srv conn =
  do serverLog srv "Requesting disconnect"
     WS.sendClose conn ("Disconnected" :: Text)

inMsg :: Server -> WithPlayer Choice -> IO ()
inMsg srv msg =
  do msgs <- serverUpate srv \srvState ->
             let (newGameState,msgs) = handleMessage msg (gameState srvState)
             in (srvState { gameState = newGameState }, msgs)
     mapM_ (outMsg srv) msgs

outMsg :: Server -> WithPlayer OutMsg -> IO ()
outMsg srv (tgt :-> msg) =
  do state <- serverState srv
     case Map.lookup tgt (connected state) of
       Just conn -> sendTo conn msg
       Nothing   -> pure ()


--------------------------------------------------------------------------------

sendTo :: JS.ToJSON a => Connection -> a -> IO ()
sendTo conn msg = sendTextData conn (JS.encode msg)

recvFromMaybe :: JS.FromJSON a => Connection -> IO (Maybe a)
recvFromMaybe conn =
  do bs <- receiveData conn
     -- serverLog srv ("Received bytes: " ++ show bs)
     pure (JS.decode bs)

