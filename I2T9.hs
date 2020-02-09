{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import Network.HTTP.Client (Manager)
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Web.Telegram.API.Bot 
import Web.Telegram.API.Bot.Responses (UpdatesResponse)
import Data.Char
-- import qualified Data.Text.IO as D
import Data.Text (pack, Text(..)) 
import System.IO hiding (getLine)
import System.Environment
import qualified Data.Text as T
-- import Web.Telegram.API.Bot.API (Message(..))
import Data.Foldable (sequenceA_)
import Control.Monad
import Control.Concurrent
import Data.List 
import qualified Control.Exception as E
-- import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as D
import qualified Data.Text.Encoding as En
import Control.Concurrent.Async
import Data.Text.Encoding.Error
import System.Exit
import qualified Data.ByteString.Lazy.UTF8 as DBU
import Data.Map.Strict

-- telegram-api for haskell https://github.com/klappvisor/haskell-telegram-api
-- howItWorks :: yourTelegramAccount -> TelegramBot -> IRC2Telegram -> IRC

-- 1. create a bot from BotFather on telegram, then get its token and your telegram account's chatId, talk to that bot
-- 2. git clone haskell https://github.com/klappvisor/haskell-telegram-api.git then cd into it, cabal v2-build, put this file into that directory
-- 3. change the default irc user info in the file, then ghc IRC2TelegramM.hs, once done, run it

-- in your telegram account conversation with your telegram bot, send messages to irc syntax: #channel msg
-- send irc commands syntax: /COMMAND PARAMETERS

-- usage: 
-- #channel message
-- /time      -- just other valid irc commands, start them with /
-- /prefix #channel
-- /prefix nick    -- it's equal to /msg or /query
-- message  -- after you use `/prefix #channel' then you can send message directly

-- /set a #channel nick1 nick2  -- then 'a messgaes' replace 'a' with '#channel nick'
--- /unset  -- clear all the alias, 'a messages' will be send as it is

-- /to #channel nick1 nick2  -- then you all next messages will send to this nick, this will set your prefix #channel
-- /clear -- remove nick preifx, and also you can do /prefix #channel to get the same effect

-----------------------------------------------------------------

-- change info here
-- irc        
_server = "irc.freenode.net"
_port = "6665"
_nick = "albet70"
_joinChannel = "#linuxba"
-- telegram 
_token = "bot9"
_chatId = 7

--------------------------------------------------------------------

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe a
tailMaybe [] = Nothing
tailMaybe xs = Just $ last xs

filterUtf8Exception :: Either UnicodeException Text -> Text
filterUtf8Exception x =
    case x of
        Right v -> v
        Left e -> "Non UTF8 character"

sleep = threadDelay . ceiling . (*1000000)

takeIt :: Maybe (Maybe Text) -> Text
takeIt Nothing = " "
takeIt (Just Nothing) = " "
takeIt (Just (Just a)) = a

-- send msg to your telegram account by this bot api
-- https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/API/Messages.hs
-- sendMessage :: Token -> SendMessageRequest -> Manager -> IO (Either ClientError MessageResponse)
-- https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/Requests.hs

--data SendMessageRequest = SendMessageRequest { message_chat_id :: ChatId,
--message_text :: Text, 
--message_parse_mode :: Maybe ParseMode,
--message_disable_web_page_preview :: Maybe Bool,
--message_disable_notification :: Maybe Bool,
--message_reply_to_message_id :: Maybe Int,
--message_reply_markup :: Maybe ReplyKeyboard
--} deriving (Show, Generic)



sendMsg :: ChatId -> Text -> Token -> Manager -> IO ()
sendMsg chatId inputMsg token manager = do
  let request = sendMessageRequest chatId inputMsg
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStr "sendMsg failed: "
      print e
    Right Response { result = m } -> do
      putStr "sent: "
      putStrLn $ (show $ message_id m) ++ " " ++ (show $ text m)

-- https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/API/Updates.hs
-- https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/Data.hs
-- data Update = Update  {
--    update_id            :: Int 
--  , message              :: Maybe Message 
--  , edited_message       :: Maybe Message 
--  , channel_post         :: Maybe Message 
--  , edited_channel_post  :: Maybe Message 
--  , inline_query         :: Maybe InlineQuery 
--  , chosen_inline_result :: Maybe ChosenInlineResult 
--  , callback_query       :: Maybe CallbackQuery 
--  , shipping_query       :: Maybe ShippingQuery 
--  , pre_checkout_query   :: Maybe PreCheckoutQuery 
-- } deriving (FromJSON, ToJSON, Show, Generic)

-- data Message = Message
--  { message_id :: Int, from :: Maybe User, date :: Int, chat :: Chat, forward_from :: Maybe User, forward_from_chat :: Maybe Chat, forward_from_message_id :: Maybe Int
--  , forward_signature :: Maybe Text, forward_date :: Maybe Int, reply_to_message :: Maybe Message, edit_date :: Maybe Int, media_group_id :: Maybe Text 
--  , author_signature :: Maybe Text, text :: Maybe Text, entities :: Maybe [MessageEntity], caption_entities :: Maybe [MessageEntity] ...}
-- https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/Data.hs

getResult :: [Update] -> [Maybe Text]
getResult  x = fmap (g . message) x
g :: Maybe Message -> Maybe Text
g (Just x) = text x
-- g Nothing = Nothing
g Nothing = Just "update message is Nothing"

-- relay Telegram bot messages to IRC
recvMsg :: Token -> Manager -> Maybe Int -> [Maybe Text] -> ChatId -> Socket -> T.Text -> Map T.Text T.Text -> IO ()
recvMsg token manager upId prevResult chatId socket defaultPrefix alistMap = do
  -- sleep 3
  -- getUpdates :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> Manager-> IO (Either ClientError UpdatesResponse)
  resultEither <- getUpdates token upId (Just 100) (Just 0) manager
  -- resultEither :: Either ClientError UpdatesResponse
  case resultEither of
        -- type UpdatesResponse = Response [Update]
        -- data Response a = Response {result :: a , parameters :: Maybe ResponseParameters} deriving (Show, Generic, FromJSON)
        Right Response { result = m }  -> do
            -- m :: [Update]
            -- updateId :: [Int]
            let updateId = fmap update_id  m
            case (tailMaybe updateId) of
                Nothing -> recvMsg token manager upId prevResult chatId socket defaultPrefix alistMap
                Just x -> do
                    let latestId = x
                    if (not (Just latestId == upId)) then do
                        result <- return $ (getResult m) Data.List.\\ prevResult  -- remove the same element in result
                        -- result :: [Maybe Text]
                        putStr "recv: "
                        sequenceA_ (fmap print result)
                        -- putStr "[1]"
                        -- print latestId
                        -- sendMsg chatId (T.reverse $ takeIt $ head result) token manager
                        -- let t2i = En.encodeUtf8 $ takeIt $ headMaybe result
                        -- t2i :: Text
                        let t2i = takeIt $ headMaybe result
                        
                        if | T.isPrefixOf "/prefix " t2i -> recvMsg token manager (Just latestId) result chatId socket (T.drop 8 t2i) alistMap
                           -- /to #channel nick1 nick2 -- drop on Text is better than drop on List, T.drop 4 t2i, t2i absolutely has '/to ', but drop 2 . words $ '/to ' may get []
                           | T.isPrefixOf "/to " t2i -> recvMsg token manager (Just latestId) result chatId socket ((Prelude.head . Prelude.tail . T.words $ t2i) <> " :" <> (T.unwords . Prelude.tail . T.words . T.drop 4 $ t2i)) alistMap
                           | "/clear" == t2i -> recvMsg token manager (Just latestId) result chatId socket (Prelude.head . T.words $ defaultPrefix) alistMap
                           | not (T.isPrefixOf "/" t2i) -> if | T.any (== ' ') t2i ->  
                                                                sendAll socket $ sendToChannel (T.unwords $ (findAlias alistMap . Prelude.head . T.words $ t2i) : (tail . T.words $ t2i)) defaultPrefix
                                                              | otherwise -> sendAll socket $ sendToChannel t2i defaultPrefix -- append prefix PRIVMSG
                           | "/" == t2i -> sendAll socket $ sendToChannel t2i defaultPrefix
                           -- /set a #channel nick1 nick2
                           | T.isPrefixOf "/set " t2i -> recvMsg token manager (Just latestId) result chatId socket defaultPrefix $ Data.Map.Strict.insert (Prelude.head . T.words . T.drop 5 $ t2i) (T.unwords . Prelude.tail . T.words . T.drop 5 $ t2i) alistMap
                           | "/unset" == t2i -> recvMsg token manager (Just latestId) result chatId socket defaultPrefix Data.Map.Strict.empty
                           | otherwise -> sendAll socket (T.drop 1 $ t2i <> "\r\n")  -- raw messages like "/TIME" or "/JOIN #channel", just drop "/" and send the rest to irc server

                        recvMsg token manager (Just latestId) result chatId socket defaultPrefix alistMap
                        
                    else do
                        sleep 1
                        recvMsg token manager (Just latestId) prevResult chatId socket defaultPrefix alistMap
                        -- now telegram api send and recv both are async, IRC send is non-blocked, only IRC recv is blocked.
                    
        Left e -> do
            putStr "recvMsg error:"
            print e

findAlias :: Map T.Text T.Text -> T.Text -> T.Text
findAlias alistMap x = if Data.Map.Strict.empty == alistMap then x else if x `notMember` alistMap then x else (alistMap ! x)

nick = "NICK " <> _nick <> "\r\n"
user = "USER xxx 8 * :xxx\r\n"
autoJoinChannel = "JOIN " <> _joinChannel <> "\r\n"

-- x is like "#channel msg", raw messages like "/JOIN #channel" or "/TIME", start with "/", just drop "/" and send the rest to irc server
sendToChannel :: T.Text -> T.Text -> D.ByteString
sendToChannel x _defaultPrefix =
              if (T.isPrefixOf "#" x) then
                  En.encodeUtf8 ("PRIVMSG " <> (Prelude.head (T.splitOn " " x)) <> " :" <> (T.unwords . Prelude.tail . T.splitOn " " $ x) <> "\r\n")
              else
                  En.encodeUtf8 ("PRIVMSG " <> _defaultPrefix <> " :" <> x <> "\r\n")

-- filter prefix messages PRIVMSG QUIT JOIN PART
parseMsg :: D.ByteString -> D.ByteString -> D.ByteString
parseMsg x nick = case (D.isInfixOf " PRIVMSG #" x) of
    True -> 
        case (D.isInfixOf nick x) of 
            False ->
                (fst (D.breakSubstring ":" (snd $ D.breakSubstring "#" x))) <>  -- channel
                (D.drop 1 (fst $ D.breakSubstring "!" x)) <> ": " <>  -- nick
                (D.drop 1 (snd $ D.breakSubstring ":" (snd $ D.breakSubstring "#" x)))  -- message
            True ->
                (En.encodeUtf8 "\128994") <> -- an emoji circle highlight, decimal
                (fst (D.breakSubstring ":" (snd $ D.breakSubstring "#" x))) <>  -- channel
                (D.drop 1 (fst $ D.breakSubstring "!" x)) <> ": " <>  -- nick
                (D.drop 1 (snd $ D.breakSubstring ":" (snd $ D.breakSubstring "#" x)))  -- message
                
    False -> 
        case (D.isInfixOf (" PRIVMSG " <> nick <> " :") x) of
            True ->
                (En.encodeUtf8 "\128994") -- highlight emoji
                <> (D.drop 1 (Prelude.head (tokenise "!" x))) -- nick
                <>  ": " <> (D.drop 2 (snd (D.breakSubstring " :" x))) -- message
            False ->

                if | D.isInfixOf " QUIT :" x -> "hide"
                   | D.isInfixOf " JOIN #" x -> "hide"
                   | D.isInfixOf " PART #" x -> "hide"
                   | (D.isInfixOf " NICK :" x) && (D.isInfixOf nick x) -> "newNick-" <>  (D.drop 7 $ snd $ D.breakSubstring " NICK :" (fst $ D.breakSubstring "\r\n" x))
                   | D.isInfixOf nick x -> (En.encodeUtf8 "\128994") <> x
                   | otherwise -> x

-- ":irc27313! ... NICK :xxx"

filterMsg x = ()

main :: IO ()
main = runTCPClient _server _port $ \socket -> do
    manager <- newManager tlsManagerSettings
    token <- return $ Token _token
    chatId <- return $ ChatId _chatId
    sendAll socket nick
    sendAll socket user
    sendAll socket autoJoinChannel

    threadId <- forkIO (recvMsg token manager Nothing [] chatId socket _nick Data.Map.Strict.empty)  -- Telegram to IRC
    relayIRC2Tele token manager chatId socket threadId _nick -- IRC to Telegram
 
tokenise :: D.ByteString -> D.ByteString -> [D.ByteString] -- splitOn for Data.ByteString
tokenise x y = h : if D.null t then [] else tokenise x (D.drop (D.length x) t)
     where (h,t) = D.breakSubstring x y
                       
-- DBSLU.toString fromString
-- main can be IO () or IO a, ghc won't complain, exitWith :: ExitCode -> IO a
relayIRC2Tele :: Token -> Manager -> ChatId -> Socket -> ThreadId -> D.ByteString -> IO a
relayIRC2Tele token manager chatId socket threadId nick = do
    msg <- recv socket 1024  -- msg :: ByteString
    if (D.length msg == 0) then do           --  irc disconnected
        print "IRC Disconnected, Re-connecting"
        killThread threadId      
        sleep 15
        -- main
        exitWith $ ExitFailure 22
    else do
        let msgList = tokenise "\r\n" msg -- msgList :: [ByteString]
        -- ["abcd","PING :xxxx","efgh",""]
        let getPING = Prelude.filter (D.isPrefixOf "PING") msgList
        if not (Prelude.null getPING) then do
            -- contain PING
            sendAll socket $ (D.map (\i -> if (i == 73) then 79 else i) (head getPING)) <> "\r\n" -- PING PONG
        else do
            return ()
        let removePING = Prelude.filter (not . (D.isPrefixOf "PING")) msgList
        if (Prelude.null removePING) then do
            -- only PING
            relayIRC2Tele token manager chatId socket threadId nick
        else do
            -- filter empty string ""
            let removeEmptyString = Prelude.filter (not . (== "")) removePING
            let parseResult = fmap (flip parseMsg nick) removeEmptyString
            let removeHide = Prelude.filter (not . (== "hide")) parseResult
            if Prelude.null removeHide then do
                relayIRC2Tele token manager chatId socket threadId nick
            else do
                let newNick = Prelude.filter (D.isPrefixOf "newNick-") removeHide
                if Prelude.null newNick then do
                    -- no new nick
                    sequenceA_ (fmap (\msg -> sendMsg chatId (filterUtf8Exception $ En.decodeUtf8' (msg <> "\r\n")) token manager) removeHide)
                    relayIRC2Tele token manager chatId socket threadId nick
                else do
                    -- there's new nick and only
                    if newNick == removeHide then do
                        relayIRC2Tele token manager chatId socket threadId $ D.drop 8 (head newNick)
                    -- there're also other messages
                    else do
                        sequenceA_ (fmap (\msg -> sendMsg chatId (filterUtf8Exception $ En.decodeUtf8' (msg <> "\r\n")) token manager) (Prelude.filter (not . (D.isPrefixOf "newNick-")) removeHide))
                        relayIRC2Tele token manager chatId socket threadId $ D.drop 8 (head newNick)

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
