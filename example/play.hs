import Control.Monad
import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent

import Sound.ProteaAudio

main = do
    args <- getArgs
    initAudio 64 44100 1024
    let fname = head args
    -- load from file
    smp <- sampleFromFile fname 1.0 -- volume

    buf <- SB.readFile fname
    -- load from memory buffer
    smp' <- case takeExtension fname of
      ".ogg" -> SB.useAsCStringLen buf $ \(p,i) -> sampleFromMemoryOgg p i 1
      ".wav" -> SB.useAsCStringLen buf $ \(p,i) -> sampleFromMemoryWav p i 1

    soundPlay smp' 1 1 0 1
    let loop = do
            n <- soundActive
            when  (n > 0) $ threadDelay 1000000 >> loop
    loop
    finishAudio
