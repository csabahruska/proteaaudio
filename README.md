# Overview

ProteaAudio is a stereo audio mixer/playback library for

- Linux *(PulseAudio)*
- Macintosh OS X *(CoreAudio)*
- Windows *(DirectSound)*

Supported audio formats:
- Wav
- Ogg
- Raw linear PCM

Samples can be loaded from file or memory.

# Example

```haskell
import Control.Monad
import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent

import Sound.ProteaAudio

waitPayback = do
  n <- soundActive
  when  (n > 0) $ do
    threadDelay 1000000
    waitPayback

main = do
    args <- getArgs
    filename <- case args of
      a : _ -> pure a
      _ -> fail "usage: proteaaudio-play SAMPLE_FILE_NAME"

    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    -- (A) load sample from file
    sampleA <- sampleFromFile filename 1.0 -- volume

    soundPlay sampleA 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    waitPayback

    -- (B) load from memory buffer
    buffer <- SB.readFile filename
    sampleB <- case takeExtension filename of
      ".ogg" -> sampleFromMemoryOgg buffer 1.0
      ".wav" -> sampleFromMemoryWav buffer 1.0

    soundPlay sampleB 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    waitPayback

    finishAudio
```
