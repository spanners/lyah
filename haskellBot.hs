{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
import System.Random
import Data.Char
import Data.List
import Control.Monad
import Network
import System.IO

pattern PBot = "HaskellBot"
pattern JOIN nick chan 
   <- (words -> [getNick -> Just nick, "JOIN", chan])
pattern PING serv <- (words -> ["PING", serv])
pattern NICK n <- ((\a -> (head a /= '#', a)) -> (True, n))
pattern CHAN c <- ((\a -> (head a == '#', a)) -> (True, c))
pattern PM from m <- (getPriv -> Just (from, NICK PBot,  m))
pattern MSG from to m <- (getPriv -> Just (from, CHAN to, m))
pattern Dog <- (isInfixOf "dog" . map toLower -> True)
pattern Command cmd = '>':' ':cmd
pattern Roll <- Command (map toLower -> "roll")

nick :: Handle -> String -> IO ()
nick h name = hPutStrLn h ("NICK " ++ name)

-- Specify username
user :: Handle -> String -> IO ()
user h name = hPutStrLn h ("USER " ++ name ++ " 0 * :" ++ name)

-- Join a channel
joinChan :: Handle -> String -> IO ()
joinChan h chan = hPutStrLn h ("JOIN " ++ chan)

main = do
  h <- connectTo "irc.freenode.org" (PortNumber 6667)
  hSetBuffering   h NoBuffering
  hSetNewlineMode h (NewlineMode CRLF CRLF)

  nick h "HaskellBot"
  user h "HaskellBot"
  
  joinChan h "##HaskellBot"

  forever (action h)

pong :: Handle -> String -> IO ()
pong h serv = hPutStrLn h ("PONG " ++ serv)

getNick :: String -> Maybe String
getNick (':':prefix) = do
  index <- findIndex (== '!') prefix
  return (take index prefix)
getNick _            = Nothing

msg :: Handle -> String -> String -> IO ()
msg h chan msg = hPutStrLn h ("PRIVMSG " ++ chan ++ " :" ++ msg)

action :: Handle -> IO ()
action h = do
  line <- hGetLine h
  case line of
    PING serv           -> pong h serv
    JOIN PBot chan      -> msg h chan "Hello, I'm a bot!"
    JOIN nick chan      -> msg h chan (nick ++ ": Welcome to " ++ chan)
    PM   from m         -> msg h from ("\"" ++ m ++ "\"... interesting...")
    MSG  from chan Dog -> msg h chan "Woof!"
    MSG  from chan Roll -> do
      roll :: Int <- randomRIO (1, 6)
      msg h chan (from ++ ": You rolled " ++ show roll)
    _                   -> return ()

getPriv :: String -> Maybe (String, String, String)
getPriv msg = case words msg of
  sender : "PRIVMSG" : target : (':':_) : _ -> do
    nick <- getNick sender
    return (nick, target, clean msg)
  _ -> Nothing
  where
  clean = tail . dropWhile (/=':') . dropWhile (/= ' ') . tail
