{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import Network.HTTP.Client (Manager)
import Network.HTTP.Client      (newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Web.Telegram.API.Bot 
import Web.Telegram.API.Bot.Responses (UpdatesResponse)
import System.IO hiding (getLine)
import System.Environment
import System.Exit
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Data.Map.Strict
import Data.Maybe
import Data.Char
import Data.Text (pack, Text(..))
import Data.Text.Encoding.Error 
import Data.Foldable (sequenceA_)
import qualified Data.Text as T
import Data.List as L
import qualified Data.ByteString as D
import qualified Data.Text.Encoding as En

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
server = "irc.freenode.net"
port = "6665"
nick = "a"
autoJoinChannel = "#l"
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
      print $ "failed to send :" <> inputMsg 
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
                        result <- return $ (getResult m) L.\\ prevResult  -- remove the same element in result
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
                           | otherwise -> sendAll socket $ En.encodeUtf8 (T.drop 1 $ t2i <> "\r\n")  -- raw messages like "/TIME" or "/JOIN #channel", just drop "/" and send the rest to irc server

                        recvMsg token manager (Just latestId) result chatId socket defaultPrefix alistMap
                        
                    else do
                        sleep 1
                        recvMsg token manager (Just latestId) prevResult chatId socket defaultPrefix alistMap
                        -- now telegram api send and recv both are async, IRC send is non-blocked, only IRC recv is blocked.
                    
        Left e -> do
            putStr "recvMsg error:"
            print e

findAlias :: Map Text Text -> Text -> Text
findAlias alistMap x = if Data.Map.Strict.empty == alistMap then x else if x `notMember` alistMap then x else (alistMap ! x)


-- x is like "#channel msg", raw messages like "/JOIN #channel" or "/TIME", start with "/", just drop "/" and send the rest to irc server
sendToChannel :: Text -> Text -> D.ByteString
sendToChannel x _defaultPrefix =
              if (T.isPrefixOf "#" x) then
                  En.encodeUtf8 ("PRIVMSG " <> (L.head . T.words $ x) <> " :" <> (T.unwords . L.tail . T.words $ x) <> "\r\n")
              else
                  En.encodeUtf8 ("PRIVMSG " <> _defaultPrefix <> " :" <> x <> "\r\n")

toText :: D.ByteString -> Text
toText = En.decodeUtf8With lenientDecode

-- :nick!~nick@1700:bef1:5e10::1 PRIVMSG #channel :words
-- :nick!~user@unaffiliated/user PRIVMSG #channel :words
-- :*.net*.split
-- :nick!user@gateway/web/irc JOIN
-- ":irc27313! ... NICK :xxx"

getElem :: Int -> [a] -> Maybe a
getElem n = fmap fst . L.uncons . L.drop n

reduce f (x:xs) = L.foldl f x xs
isContained :: [Text] -> Text -> Bool
isContained xs msg = reduce (&&) . fmap (`T.isInfixOf` msg) $ xs

-- parsePRIVMSG ":nick!~user@addr PRIVMSG #channel :words" == Just "#channel nick :words"
parsePRIVMSG :: Text -> Maybe Text
parsePRIVMSG x = if isContained ["!","@"," PRIVMSG "] x then
                 (getElem 2 . T.words $ x) -- channel
                 <> (Just . T.replace ":" " " . L.head . T.splitOn "!" $ x) -- nick
                 <> (Just " ") <> (Just . T.unwords . L.drop 3 . T.words $ x) -- words
                 else Just x

-- filter prefix messages PRIVMSG QUIT JOIN PART
parseMsg :: Text -> Text -> Maybe Text
parseMsg nick x = 
    if | isContained ["!", "@", " PRIVMSG #"] x -> if | T.isInfixOf nick x -> Just "\128994" <> (parsePRIVMSG x)
                                                      | otherwise -> parsePRIVMSG x
       | isContained ["!", "@", " PRIVMSG "] x -> Just "\128994" <>  ((T.unwords . L.tail . T.words) <$> (parsePRIVMSG x)) -- in case raw cmd help "PRIVMSG JOIN..."
       | T.isPrefixOf "PING " x -> Nothing
       | isContained ["!", "@", " QUIT"] x -> Nothing
       | isContained ["!", "@", " JOIN #"] x -> Nothing
       | isContained ["!", "@", " PART #"] x -> Nothing
       | T.isInfixOf ":*.net*.split" x -> Nothing
       | isContained ["!", "@", " NICK :", nick] x -> Just "newNick" <> (getElem 2 . T.words $ x) -- Just "newNick:nick"
       | T.isInfixOf nick x -> Just $ "\128994" <> x
       | otherwise -> Just x

msgList :: D.ByteString -> [Text]
msgList = T.splitOn "\r\n" . toText

relayIRC2Tele :: Token -> Manager -> ChatId -> Socket -> ThreadId -> Text -> IO a
relayIRC2Tele token manager chatId socket threadId nick = do
    msg <- recv socket 1024  -- msg :: ByteString
    if | D.length msg == 0 -> do           --  irc disconnected
                        print "IRC Disconnected, Re-connecting"
                        killThread threadId      
                        sleep 15
                        -- main
                        exitWith $ ExitFailure 22 -- main can be IO () or IO a
       | L.all (T.isPrefixOf "PING ") $ msgList msg -> do -- only contain PING msg
               sendAll socket $ (En.encodeUtf8 . T.replace "PING" "PONG" . L.head . msgList $ msg) <> "\r\n"
               relayIRC2Tele token manager chatId socket threadId nick               

       | L.any (T.isPrefixOf "PING") $ msgList msg -> do -- contain PING msg
               sendAll socket $ (En.encodeUtf8 . T.replace "PING" "PONG" . L.head . L.filter (T.isPrefixOf "PING") $ msgList msg) <> "\r\n"
               let parsedList = catMaybes . fmap (parseMsg nick) $ msgList msg
               if | L.null parsedList -> relayIRC2Tele token manager chatId socket threadId nick -- only contain PING or PART or JOIN sort of messages
                  | not . L.any (T.isPrefixOf "newNick:") $ parsedList -> do -- do not contain new nick
                            sequenceA_ . fmap (\m -> sendMsg chatId (m <> "\r\n") token manager) $ parsedList
                            relayIRC2Tele token manager chatId socket threadId nick
                  | L.all (T.isPrefixOf "newNick:") parsedList -> -- only contain new nick
                            relayIRC2Tele token manager chatId socket threadId (T.drop 8 . L.head $ parsedList)
                  | otherwise -> do -- contain new nick and other messages
                            sequenceA_ . fmap (\m -> sendMsg chatId (m <> "\r\n") token manager) . L.filter (not . T.isPrefixOf "newNick:") $ parsedList
                            relayIRC2Tele token manager chatId socket threadId (T.drop 8 . L.head . L.filter (T.isPrefixOf "newNick:") $ parsedList)
       | otherwise -> 
               let parsedList = catMaybes . fmap (parseMsg nick) $ msgList msg
               if | L.null parsedList -> relayIRC2Tele token manager chatId socket threadId nick -- only contain PING or PART or JOIN sort of messages
                  | not . L.any (T.isPrefixOf "newNick:") $ parsedList -> do -- do not contain new nick
                            sequenceA_ . fmap (\m -> sendMsg chatId (m <> "\r\n") token manager) $ parsedList
                            relayIRC2Tele token manager chatId socket threadId nick
                  | L.all (T.isPrefixOf "newNick:") parsedList -> -- only contain new nick
                            relayIRC2Tele token manager chatId socket threadId (T.drop 8 . L.head $ parsedList)
                  | otherwise -> do -- contain new nick and other messages
                            sequenceA_ . fmap (\m -> sendMsg chatId (m <> "\r\n") token manager) . L.filter (not . T.isPrefixOf "newNick:") $ parsedList
                            relayIRC2Tele token manager chatId socket threadId (T.drop 8 . L.head . L.filter (T.isPrefixOf "newNick:") $ parsedList)

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

nickCmd = "NICK " <> nick <> "\r\n"
userCmd = "USER xxx 8 * :xxx\r\n"
autoJoinChannelCmd = "JOIN " <> autoJoinChannel <> "\r\n"

main :: IO ()
main = runTCPClient server port $ \socket -> do
    manager <- newManager tlsManagerSettings
    token <- return $ Token _token
    chatId <- return $ ChatId _chatId
    sendAll socket $ En.encodeUtf8 nickCmd
    sendAll socket $ En.encodeUtf8 userCmd
    sendAll socket $ En.encodeUtf8 autoJoinChannelCmd

    threadId <- forkIO (recvMsg token manager Nothing [] chatId socket nick Data.Map.Strict.empty)  -- Telegram to IRC
    relayIRC2Tele token manager chatId socket threadId nick -- IRC to Telegram
