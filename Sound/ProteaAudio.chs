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

newtype Sample = Sample {#type sample_t#}

toSample s = Sample s
fromSample (Sample s) = s

{#fun initAudio {`Int', `Int', `Int'} -> `Bool'#}
{#fun finishAudio {} -> `()'#}
{#fun loaderAvailable {`String'} -> `Bool'#}
{#fun sampleFromMemoryWav {id `Ptr CChar', `Int', `Float'} -> `Sample' toSample#}
{#fun sampleFromMemoryOgg {id `Ptr CChar', `Int', `Float'} -> `Sample' toSample#}
{#fun sampleFromFile {`String', `Float'} -> `Sample' toSample#}

{#fun volume {`Float', `Float'} -> `()'#}
{#fun soundActive {} -> `Int'#}
{#fun soundStopAll {} -> `()'#}

{#fun soundLoop {fromSample `Sample', `Float', `Float', `Float', `Float'} -> `()'#}
{#fun soundPlay {fromSample `Sample', `Float', `Float', `Float', `Float'} -> `()'#}
{#fun soundUpdate {fromSample `Sample', `Float', `Float', `Float', `Float'} -> `Bool'#}
{#fun soundStop {fromSample `Sample'} -> `Bool'#}
