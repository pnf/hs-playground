import Control.Concurrent
import Control.Monad
import System.IO
import GetURL
import Data.ByteString as B
import GetURL
import Text.Printf

main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')
  
mainRemind =
  forever $ do
    s <- getLine
    forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Please wait %d seconds\n" t

mainMVar = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "Logger: stop"
          putMVar s ()
 

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s
  
mainLogger :: IO ()
mainLogger = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chann a
  = Chann (MVar (Stream a))
         (MVar (Stream a))

newChan :: IO (Chann a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chann readVar writeVar)

  
    
