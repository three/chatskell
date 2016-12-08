{-# LANGUAGE OverloadedStrings #-}

module Chatskell (
    runChatskell
) where

-- Warp and dependencies
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

-- Helpers
import Control.Concurrent.MVar            (takeMVar, readMVar,
                                           putMVar, newMVar, MVar)
import Data.ByteString                    (ByteString,readFile)
import Data.ByteString.Builder            (byteString)
import Data.List                          (find)
import Data.Maybe                         (listToMaybe)
import Data.Monoid                        ((<>))
import Data.Text as T                     (pack,unpack)

-- Modules
import AssetLoader

-- App State Management

data ChatState = ChatState {
    stateMessages :: MVar ByteString
    } deriving (Eq)

initChatState :: IO ChatState
initChatState = newMVar "-- START --\n"
    >>= (return.ChatState)

logRequest :: Request -> Status -> Maybe Integer -> IO ()
logRequest _ s _  = putStrLn.show.statusMessage $ s

-- Helper Data

serverSettings :: Settings
serverSettings = setLogger logRequest $ defaultSettings

apiHeaders :: ResponseHeaders
apiHeaders =
    [(hContentType, "text/plain")]

htmlHeaders :: ResponseHeaders
htmlHeaders =
    [(hContentType, "text/html")]

-- Routes

routeRoot :: ChatState -> Application
routeRoot state = routeFile "chat.html"

routeStatic :: ChatState -> Application
routeStatic state req = routeFile fname req
    where
        rPath = map T.unpack $ pathInfo req
        fname =
            if (length rPath)==2 then rPath!!1
            else "" -- This will always be a bad path
                    -- without the pain of using Maybe

routeMessages :: ChatState -> Application
routeMessages state req res = do
    chatlog <- (readMVar.stateMessages) state
    res $ responseBuilder
        ok200 apiHeaders
        (byteString chatlog)

routeSay :: ChatState -> Application
routeSay state req res =
    case msg of
        Just m -> do
            chatlog <- (takeMVar.stateMessages) state
            (putMVar.stateMessages) state $
                chatlog <> m <> "\n"
            res $ responseBuilder
                ok200 apiHeaders
                "S"
        Nothing -> res $ responseBuilder
            badRequest400 []
            "ERROR: BAD REQUEST 400"
    where msg = (>>=snd) . find ((=="c").fst) $
            queryString req

-- Helper Routes

routeFile :: [Char] -> Application
routeFile fname req res = case getAsset fname of
    Just (fType, fCont) -> res $ responseBuilder ok200
        [(hContentType, fType)] (byteString fCont)
    Nothing -> route404 req res

route404 :: Application
route404 req res = res $ responseBuilder
    notFound404 apiHeaders
    "ERROR: NOT FOUND 404"

-- Application

app :: ChatState -> Application
app state req =
    -- Very primitive routing framework with case statements
    case (listToMaybe $ pathInfo req) of
        -- /
        Nothing -> routeRoot state req
        -- /static/
        Just "static" -> routeStatic state req
        -- /messages/
        Just "messages" -> routeMessages state req
        -- /say/
        Just "say" -> routeSay state req
        -- 404 Not Found
        otherwise -> route404 req

runChatskell :: IO ()
runChatskell = do
    state <- initChatState
    runSettings serverSettings $ app state
