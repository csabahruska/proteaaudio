{-|
    ProteaAudio is a stereo audio mixer/playback library
    for Linux (native ALSA, Jack, and OSS),
    Macintosh OS X (CoreAudio and Jack),
    and Windows (DirectSound and ASIO) operating systems.
-}
{-#LANGUAGE ForeignFunctionInterface#-}
#include "proteaaudio_binding.h"
module Sound.ProteaAudio (
    initAudio,
    finishAudio,
    loaderAvailable,
    volume,
    sampleFromMemoryWav,
    sampleFromMemoryOgg,
    sampleFromFile,
    soundActive,
    soundStopAll,
    soundLoop,
    soundPlay,
    soundUpdate,
    soundStop,
    Sample()
 ) where

import Foreign
import Foreign.C
import Data.ByteString (ByteString, useAsCStringLen)

-- | audio sample handle
newtype Sample = Sample {#type sample_t#}

toSample s = Sample s
fromSample (Sample s) = s

-- | initializes the audio system
{#fun initAudio
    { `Int' -- ^ the maximum number of sounds that are played parallely. Computation time is linearly correlated to this factor.
    , `Int' -- ^ sample frequency of the playback in Hz. 22050 corresponds to FM radio 44100 is CD quality. Computation time is linearly correlated to this factor.
    , `Int' -- ^ the number of bytes that are sent to the sound card at once. Low numbers lead to smaller latencies but need more computation time (thread switches). If a too small number is chosen, the sounds might not be played continuously. The default value 512 guarantees a good latency below 40 ms at 22050 Hz sample frequency.
    } -> `Bool' -- ^ returns True on success
#}

-- | releases the audio device and cleans up resources
{#fun finishAudio {} -> `()'#}

-- | checks if loader for this file type is available
{#fun loaderAvailable
  { `String' -- ^ file extension (e.g. ogg)
  } -> `Bool'
#}

-- | loads wav sound sample from memory buffer
{#fun _sampleFromMemoryWav
  { id `Ptr CChar' -- ^ memory buffer pointer
  , `Int' -- ^ memory buffer size in bytes
  , `Float' -- ^ volume
  } -> `Sample' toSample -- ^ returns handle
#}

-- | loads ogg sound sample from memory buffer
{#fun _sampleFromMemoryOgg
  { id `Ptr CChar' -- ^ memory buffer pointer
  , `Int' -- ^ memory buffer size in bytes
  , `Float' -- ^ volume
  } -> `Sample' toSample -- ^ returns handle
#}

-- | loads wav sound sample from memory buffer
sampleFromMemoryWav :: ByteString -- ^ wav sample data
                    -> Float -- ^ volume
                    -> IO Sample -- ^ return sample handle
sampleFromMemoryWav wavData volume = useAsCStringLen wavData $ \(ptr, size) -> _sampleFromMemoryWav ptr size volume

-- | loads ogg sound sample from memory buffer
sampleFromMemoryOgg :: ByteString -- ^ ogg sample data
                    -> Float -- ^ volume
                    -> IO Sample -- ^ return sample handle
sampleFromMemoryOgg oggData volume = useAsCStringLen oggData $ \(ptr, size) -> _sampleFromMemoryOgg ptr size volume

-- | loads a sound sample from file
{#fun sampleFromFile
  { `String' -- ^ sample filepath
  , `Float' -- ^ volume
  } -> `Sample' toSample -- ^ returns handle
#}

-- | set mixer volume
{#fun volume
  { `Float' -- ^ left
  , `Float' -- ^ right
  } -> `()'
#}

-- | number of currently active sounds
{#fun soundActive {} -> `Int'#}

-- | stops all sounds immediately
{#fun soundStopAll {} -> `()'#}

-- | plays a specified sound sample continuously and sets its parameters
{#fun soundLoop
  { fromSample `Sample' -- ^ handle of a previously loaded sample
  , `Float' -- ^ left volume
  , `Float' -- ^ right volume
  , `Float' -- ^ time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right
  , `Float' -- ^ pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample
  } -> `()'
#}

-- | plays a specified sound sample once and sets its parameters
{#fun soundPlay
  { fromSample `Sample' -- ^ handle of a previously loaded sample
  , `Float' -- ^ left volume
  , `Float' -- ^ right volume
  , `Float' -- ^ time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right
  , `Float' -- ^ pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample
  } -> `()'
#}

-- | updates parameters of a specified sound
{#fun soundUpdate
  { fromSample `Sample' -- ^ handle of a currently active sound
  , `Float' -- ^ left volume
  , `Float' -- ^ right volume
  , `Float' -- ^ time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right
  , `Float' -- ^ pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample
  } -> `Bool' -- ^ return True in case the parameters have been updated successfully
#}

-- | stops a specified sound immediately
{#fun soundStop {fromSample `Sample'} -> `Bool'#}
