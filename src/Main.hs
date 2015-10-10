{-# LANGUAGE OverloadedStrings #-}

import qualified System.Process as P
import qualified System.Process.Internals as PI
import qualified System.Locale as SL
import qualified Data.Time.Clock as C
import qualified Data.Time.Format as TF
import qualified Data.Time.Calendar as CA
import qualified System.IO as IO
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Control.Concurrent.Async as CCA
import qualified Control.Monad as CM
import qualified System.Environment as SE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder as BSB
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Monad.IO.Class as MIO
import qualified Network.HTTP.Client.TLS as TLS
import qualified Control.Exception.Base as E

expectedText :: [T.Text]
expectedText = ["96 37 f0 : 10010110 00110111 11110000", "34 e4 00 : 00110100 11100100 00000000", "34 e4 01"]

rtlProcess :: String -> PI.CreateProcess
rtlProcess rtl433Location = (P.proc rtl433Location ["-a"]) { P.std_out = P.CreatePipe, P.std_err = P.CreatePipe }

processLine :: IO () -> IO.Handle -> C.UTCTime -> IO ()
processLine action handle lastProcessed = do
  line <- IO.hGetLine handle
  --putStrLn $ "Output from hGetLine: '" ++ line ++ "'"
  let lineAsText = T.pack line
  let containsExpected = L.foldl' (\working -> \text -> working || (T.isInfixOf text lineAsText)) False expectedText
  --putStrLn $ show containsExpected
  currentTime <- C.getCurrentTime
  --putStrLn $ show currentTime
  let difference = C.diffUTCTime currentTime lastProcessed
  --putStrLn $ show difference
  let longEnoughPassed = 5.0 < (realToFrac difference)
  --putStrLn $ show longEnoughPassed
  let doorBellPressed = containsExpected && longEnoughPassed
  if doorBellPressed then action else return ()
  --if doorBellPressed then putStrLn "Magic!" else return ()
  processLine action handle $ if doorBellPressed then currentTime else lastProcessed

playDoorBellSound :: IO ()
playDoorBellSound = do
  P.callProcess "mpg123" ["doorbell.mp3"]

catcher :: E.IOException -> IO ()
catcher = print

sendPushOverMessage :: T.Text -> T.Text -> IO ()
sendPushOverMessage applicationKey userKey = do
  putStrLn "Sending message to Pushover."
  manager <- HTTP.newManager TLS.tlsManagerSettings
  basicRequest <- HTTP.parseUrl "https://api.pushover.net/1/messages.json"
  let queryText = [("token", Just applicationKey), ("user", Just userKey), ("message", Just "Ding! Dong!")]
  let request = basicRequest
                  { HTTP.method = "POST",
                    HTTP.queryString = BSB.toByteString $ HT.renderQueryText False queryText
                  }
  HTTP.httpNoBody request manager
  putStrLn "Sent message to Pushover."

sendDingDong :: IO () -> IO ()
sendDingDong sender = do
  pushWait <- CCA.async $ E.catch sender catcher
  playWait <- CCA.async $ E.catch playDoorBellSound catcher
  CCA.wait pushWait
  CCA.wait playWait

runApplication :: T.Text -> T.Text -> String -> IO ()
runApplication applicationKey userKey rtl433Location = do
  putStrLn "Starting up!"
  (_, _, Just outHandle, _) <- P.createProcess (rtlProcess rtl433Location)
  IO.hSetBuffering outHandle IO.LineBuffering
  let sendMessage = sendPushOverMessage applicationKey userKey
  processLine (sendDingDong sendMessage) outHandle $ C.UTCTime (CA.ModifiedJulianDay 0) 0

main :: IO ()
main = do
  pushoverApplicationKey <- SE.getEnv "PUSHOVER_APPLICATION_KEY"
  pushoverUserKey <- SE.getEnv "PUSHOVER_USER_KEY"
  rtl433Location <- SE.getEnv "RTL433_LOCATION"
  let app = runApplication (T.pack pushoverApplicationKey) (T.pack pushoverUserKey) rtl433Location
  E.catch app catcher

{-
main :: IO ()
main = do
  pushoverApplicationKey <- SE.getEnv "PUSHOVER_APPLICATION_KEY"
  pushoverUserKey <- SE.getEnv "PUSHOVER_USER_KEY"
  let sendMessage = sendPushOverMessage (T.pack pushoverApplicationKey) (T.pack pushoverUserKey)
  sendDingDong sendMessage
-}