{-# LANGUAGE OverloadedStrings #-}

import qualified System.Process as P
import qualified System.Process.Internals as PI
import qualified System.Locale as SL
import qualified Data.Time.Clock as C
import qualified Data.Time.Format as TF
import qualified Data.Time.Calendar as CA
import qualified Graphics.V4L2 as V4L2
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

format :: V4L2.ImageFormat
format = V4L2.ImageFormat 1024 768 V4L2.PixelJPEG V4L2.FieldNone 0 1000000 V4L2.ColorJPEG

frameHandler :: String -> String -> (Maybe T.Text -> IO ()) -> V4L2.Device -> IO ()
frameHandler s3Region s3Bucket imageSender device = 
  V4L2.withFrame device format (\ptr -> \size -> do
    currentTime <- C.getCurrentTime
    let filename = (TF.formatTime SL.defaultTimeLocale "%c" currentTime) ++ ".jpg"
    let s3HttpUrl = "http://s3-" ++ s3Region ++ ".amazonaws.com/" ++ s3Bucket ++ "/" ++ filename
    let writeAndSendFile = do
                              putStrLn "Pushing file."
                              IO.withBinaryFile filename IO.WriteMode (\handle -> IO.hPutBuf handle ptr size)
                              let s3Filename = "s3://" ++ s3Bucket ++ "/" ++ filename
                              P.callProcess "aws" ["--region", s3Region, "s3", "cp", filename, s3Filename]
                              putStrLn "Pushed file."

    sendWait <- CCA.async writeAndSendFile
    pushWait <- CCA.async $ imageSender $ Just $ T.pack s3HttpUrl
    CCA.wait sendWait
    CCA.wait pushWait
    )

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

catcher :: E.IOException -> IO ()
catcher = print

sendPushOverMessage :: T.Text -> T.Text -> Maybe T.Text -> IO ()
sendPushOverMessage applicationKey userKey imageUrl = do
  putStrLn "Sending message to Pushover."
  manager <- HTTP.newManager TLS.tlsManagerSettings
  basicRequest <- HTTP.parseUrl "https://api.pushover.net/1/messages.json"
  let imageUrlAsList = M.maybeToList $ fmap (\url -> ("url", Just url)) imageUrl
  let queryText = [("token", Just applicationKey), ("user", Just userKey), ("message", Just "Ding! Dong!")] ++ imageUrlAsList
  let request = basicRequest 
                  { HTTP.method = "POST",
                    HTTP.queryString = BSB.toByteString $ HT.renderQueryText False queryText
                  }
  HTTP.httpNoBody request manager
  HTTP.closeManager manager
  putStrLn "Sent message to Pushover."

sendDingDong :: (V4L2.Device -> IO ()) -> String -> (Maybe T.Text -> IO ()) -> IO ()
sendDingDong frameAction cameraDevice sender = do
  let failHandler = catcher >> (\_ -> sender Nothing)
  E.catch (V4L2.withDevice cameraDevice frameAction) failHandler

runApplication :: T.Text -> T.Text -> String -> String -> String -> String -> IO ()
runApplication applicationKey userKey rtl433Location cameraDevice s3Region s3Bucket = do
  putStrLn "Starting up!"
  (_, _, Just outHandle, _) <- P.createProcess (rtlProcess rtl433Location)
  IO.hSetBuffering outHandle IO.LineBuffering
  let sendMessage = sendPushOverMessage applicationKey userKey
  processLine (sendDingDong (frameHandler s3Region s3Bucket sendMessage) cameraDevice sendMessage) outHandle $ C.UTCTime (CA.ModifiedJulianDay 0) 0

main :: IO ()
main =
  do pushoverApplicationKey <- SE.getEnv "PUSHOVER_APPLICATION_KEY"
     pushoverUserKey <- SE.getEnv "PUSHOVER_USER_KEY"
     rtl433Location <- SE.getEnv "RTL433_LOCATION"
     cameraDevice <- SE.getEnv "CAMERA_DEVICE"
     s3Region <- SE.getEnv "S3_REGION"
     s3Bucket <- SE.getEnv "S3_BUCKET"
     let app = runApplication (T.pack pushoverApplicationKey) (T.pack pushoverUserKey) rtl433Location cameraDevice s3Region s3Bucket
     CM.forever $ E.catch app catcher

