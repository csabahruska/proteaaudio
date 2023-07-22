# 1.0.0
- add MP3 support

# 0.9.4
- fix PulseAudio default output device detection

# 0.9.3
- apply migration to GHC 9.4.* for Windows (see https://gitlab.haskell.org/ghc/ghc/-/issues/22738)

# 0.9.2
- fix a sample format conversion bug that caused pitch problems in `sampleFromMemoryPcm`

# 0.9.1
- fix a Cabal C/C++ compiler flag handling bug, now should compile with GHC 8.10 and above also.

# 0.9.0
- rename `soundActive` to `soundActiveAll`
- `soundActive` now queries a specific sound

# 0.8.1
- update RtAudio to 5.1.0
- update stb_vorbis to v1.19
- add sampleDestroy
- add soundPlayOn and soundLoopOn
- fix: generate unique handles for Sample and Sound types

# 0.8.0
- fix: distinct data and playback handles
- introduce Sound handle type, an abstraction for playback audio track
- soundLoop, soundPlay return a Sound handle
- soundUpdate, soundStop take a Sound handle parameter

# 0.7.1.0
- add sampleFromMemoryPcm
- update stb_vorbis to v1.14

# 0.7.0.1
- better haddock

# 0.7.0
- use ByteString for in-memory sample loading
- fix: c++ mixer init caused segfaults sometimes
- use PulseAudio backend on Linux
