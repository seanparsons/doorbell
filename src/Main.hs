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
import qualified Control.Concurrent as CC
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
expectedText = ["96 37 f0 : 10010110 00110111 11110000", "34 e4 00 : 00110100 11100100 00000", "34 e4 01"]

rtlProcess :: PI.CreateProcess
rtlProcess = (P.proc "rtl_433" ["-a"]) { P.std_out = P.CreatePipe, P.std_err = P.CreatePipe }

processLine :: IO () -> IO.Handle -> C.UTCTime -> IO ()
processLine action handle lastProcessed = do
  line <- IO.hGetLine handle
  putStrLn $ "Output from hGetLine: '" ++ line ++ "'"
  let lineAsText = T.pack line
  let containsExpected = L.any (\text -> T.isInfixOf text lineAsText) expectedText
  CM.when containsExpected $ do
    currentTime <- C.getCurrentTime
    let difference = C.diffUTCTime currentTime lastProcessed
    let longEnoughPassed = 5.0 < (realToFrac difference)
    let doorBellPressed = containsExpected && longEnoughPassed
    if doorBellPressed then action else return ()
    processLine action handle $ if doorBellPressed then currentTime else lastProcessed
  CM.unless containsExpected $ do
    processLine action handle lastProcessed

playDoorBellSound :: IO ()
playDoorBellSound = CM.void $ CC.forkIO $ do
  time <- C.getCurrentTime
  putStrLn ("Doorbell pressed at " ++ (show time))
  P.callProcess "ffplay" ["-autoexit", "-nodisp", "-loglevel", "quiet", "doorbell.mp3"]

catcher :: E.IOException -> IO ()
catcher = print

runApplication :: IO ()
runApplication = do
  putStrLn "Starting up!"
  (_, _, Just outHandle, _) <- P.createProcess rtlProcess
  IO.hSetBuffering outHandle IO.LineBuffering
  processLine playDoorBellSound outHandle $ C.UTCTime (CA.ModifiedJulianDay 0) 0

main :: IO ()
main = do
  let app = runApplication
  E.catch app catcher
