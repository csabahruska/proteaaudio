import Control.Monad
import System.Environment
import System.Posix.Unistd

import ProteaAudio

main = do
    args <- getArgs
    initAudio 8 44100 1024
    smp <- sampleFromFile (head args) 1
    soundPlay smp 1 1 0 1
    let loop = do
            n <- soundActive
            when  (n > 0) $ sleep 1 >> loop
    loop
    finishAudio
