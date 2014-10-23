{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}
import           Debug.Trace
import           Text.Printf
import qualified Data.Maybe                    as M
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Network.Http.Client           as HTTP
import           System.FilePath    (splitPath)
import           System.Environment (getArgs)
import qualified System.Time                   as T
import qualified System.IO                     as IO
import qualified System.IO.Streams             as S
import qualified System.IO.Streams.Combinators as SC

progressBar :: Int -> Int -> Int -> String
progressBar tot prog len = "[" ++ map outchar [1..len] ++ "]"
  where cur = 1 + (prog * len `div` tot) :: Int
        outchar n | n >  cur = ' '
                  | n == cur = '>'
                  | n <  cur = '='

type OutBS = S.OutputStream BS.ByteString
type ProgressFunc = Int -> Int -> Int -> String
type StreamTrans = (OutBS -> IO ()) -> (OutBS -> IO ())

withProgressBar :: ProgressFunc -> Int -> StreamTrans
withProgressBar progressFunc length outHandler outStream = do
    T.TOD secs _ <- T.getClockTime
    (fmap fst $ SC.outputFoldM printProgress (0, 0, secs) outStream) >>= outHandler
  where
    printProgress :: (Int, Int, Integer) -> BS.ByteString -> IO (Int, Int, Integer)
    printProgress (lastSize, snapSize, lastSecs) bs = do
      let !progress = lastSize + BS.length bs
      T.TOD currSecs _ <- T.getClockTime
      if currSecs > lastSecs || progress == length
        then do
          let outstr = '\r' : progressFunc length progress (progress-snapSize)
          IO.hPutStr IO.stderr outstr >> IO.hFlush  IO.stderr
          return (progress, progress, currSecs)
        else do
          return (progress, snapSize, currSecs)

normSize :: Int -> String
normSize size = (\(f,s) -> prettyFloat 4 f ++ s ) $ foldr findSize (floatSize, "") isoSizes
  where
    floatSize = fromIntegral size :: Float
    prettyFloat n f = (\s -> if last s == '.' then init s else s) $ take n $ show f
    isoSizes = reverse $ zipWith (,) (map (10**) [3,6..]) ["K","M","G","T"] :: [(Float, String)]
    findSize (lsz, lst) acc = if lsz < floatSize then (floatSize / lsz, lst) else acc

status :: ProgressFunc
status tot prog bytes = printf "%s %sb/%sb [%sb/s]   " bar (normSize prog) (normSize tot) (normSize bytes)
  where bar = progressBar tot prog 50

maybeProgress :: Maybe BS.ByteString -> StreamTrans
maybeProgress = M.maybe id (withProgressBar status . fromMaybeLength)
  where fromMaybeLength = fst . M.fromJust . C8.readInt

downloadFile :: HTTP.URL -> FilePath -> IO ()
downloadFile url name = HTTP.get url $ \response inStream -> do
  let totalLength = HTTP.getHeader response "Content-Length" :: Maybe BS.ByteString
  case HTTP.getStatusCode response of
    200  -> S.withFileAsOutput name $ maybeProgress totalLength $ S.connect inStream
    code -> error $ "Failed to download " ++ name ++ ": http response returned " ++ show code

main = do
  args <- getArgs
  case args of
    []          -> print "Specify a file to download"
    url:[]      -> downloadFile (C8.pack url) (last $ splitPath url)
    url:file:[] -> downloadFile (C8.pack url) file
    _           -> print "Usage:\n\nhget url [filename]"

  -- downloadFile "http://mirror.internode.on.net/pub/videos/linus-dunked.avi" "linus-dunked.avi"