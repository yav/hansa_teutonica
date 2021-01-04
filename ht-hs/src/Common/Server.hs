module Common.Server where

import qualified Data.ByteString.Char8 as BS8
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import Data.IORef(IORef,newIORef,readIORef,atomicModifyIORef')
import Control.Exception(catch,SomeException(..))

import qualified Data.Aeson as JS
import Network.WebSockets(Connection,sendTextData,receiveData)
import qualified Network.WebSockets as WS

import System.FastLogger(Logger,logMsg,newLogger)
import qualified Snap.Http.Server as Snap
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS

import Common.Basics
import Common.Interact

{-
appMain :: IO gs -> IO ()
appMain =
  do args <- getArgs
     srv <- case args of
              b : ps ->
                case Map.lookup (Text.pack b) boards of
                  Just board ->
                    do rng <- newTFGen
                       let mkP = PlayerId . Text.pack
                           players = Set.fromList (map mkP ps)
                       newServer players (initialGameState rng board players)
                  Nothing -> fail "unknown board"
              _ -> fail "Usage: board_name player1 player2 ..."

     Snap.quickHttpServe $ WS.runWebSocketsSnap \pending ->
       do conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) (newClient srv conn)

-}

--------------------------------------------------------------------------------

data Server app = Server
  { serverRef    :: IORef (ServerState app)
  , serverLogger :: Logger
  }

data ServerState app = ServerState
  { connected :: Map PlayerId Connection
  , gameState :: InteractState app
  }

newServer :: App app => Set PlayerId -> app -> Interact app () -> IO ()
newServer ps s doInit =
  do ref <- newIORef ServerState { connected = Map.empty
                                 , gameState = startGame ps s doInit
                                 }
     logger <- newLogger "-"
     let srv = Server { serverRef = ref, serverLogger = logger }
     Snap.quickHttpServe $ WS.runWebSocketsSnap \pending ->
       do conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) (newClient srv conn)

serverLog :: Server app -> String -> IO ()
serverLog server = logMsg (serverLogger server) . BS8.pack

serverUpate :: Server app -> (ServerState app -> (ServerState app,a)) -> IO a
serverUpate srv = atomicModifyIORef' (serverRef srv)

serverUpate_ :: Server app-> (ServerState app -> ServerState app) -> IO ()
serverUpate_ srv f = serverUpate srv \s -> (f s, ())

serverState :: Server app -> IO (ServerState app)
serverState srv = readIORef (serverRef srv)

--------------------------------------------------------------------------------


newClient :: App app => Server app -> Connection -> IO ()
newClient srv conn =
  do serverLog srv "New connection requested, waiting for ID"
     who <- PlayerId <$> receiveData conn
     mbErr <- serverUpate srv \state ->
             if Map.member who (connected state)
                then ( state, Just "player already connected")
                -- XXX: check only approved players connect?
                -- (i.e., no spectators)
                else ( state { connected =
                                      Map.insert who conn (connected state) }
                           , Nothing
                           )
     case mbErr of
       Nothing ->
         do serverLog srv ("Player connected: " ++ show who)
            clientLoop srv who conn
       Just err -> disconnect srv conn Nothing err


clientLoop :: App app => Server app -> PlayerId -> Connection -> IO ()
clientLoop srv who conn =
  loop `catch` \e -> disconnect srv conn (Just who)
                                         (show (e :: SomeException))
  where
  loop =
    do bytes <- receiveData conn
       case JS.decode bytes of
         Nothing  -> fail "invalid request"
         Just msg -> inMsg srv (who :-> msg) >> loop

disconnect :: Server app -> Connection -> Maybe PlayerId -> String -> IO ()
disconnect srv conn mbWho reason =
  do serverLog srv ("Requesting disconnect: " <> reason)
     case mbWho of
       Just who ->
         serverUpate_ srv \state ->
                           state { connected =
                                        Map.delete who (connected state) }
       Nothing -> pure ()
     WS.sendClose conn ("Disconnected" :: Text)
        `catch` \SomeException {} -> pure ()

inMsg :: App app => Server app -> WithPlayer (PlayerRequest app) -> IO ()
inMsg srv msg =
  do msgs <- serverUpate srv \srvState ->
             let (newGameState,msgs) = handleMessage msg (gameState srvState)
             in (srvState { gameState = newGameState }, msgs)
     mapM_ (outMsg srv) msgs

outMsg :: App app => Server ap -> WithPlayer (OutMsg app) -> IO ()
outMsg srv (tgt :-> msg) =
  do state <- serverState srv
     case Map.lookup tgt (connected state) of
       Just conn ->
        sendTextData conn (JS.encode msg)
          `catch` \e -> disconnect srv conn (Just tgt)
                                            (show (e :: SomeException))
       Nothing   -> pure ()



