import Control.Concurrent.Chan
import Data.Monoid

newtype WriterRef a = WriterRef (Chan a)

write :: WriterRef a -> a -> IO ()
write (WriterRef chan) = writeChan chan

new :: (Monoid a) => IO (a, WriterRef a)
new = do
    chan <- newChan
    contents <- getChanContents chan
    return (mconcat contents, WriterRef chan)
