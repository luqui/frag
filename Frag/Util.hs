module Frag.Util where

import Frag.Time
import Frag.Behavior
import Data.Time.Clock
import Frag.RWChan
import Data.Maybe
import Control.Applicative

makeBehaviorMonitor :: Behavior a -> IO (IO (), IO a)
makeBehaviorMonitor beh = do
    (times,timechan) <- newWChan
    (thread, vals) <- evalBehavior beh times
    vchan <- newRChan vals
    
    let monitor = do
            now <- getCurrentTime
            writeWChan timechan (exact now)
            fromJust <$> readRChan vchan
    return (thread, monitor)
