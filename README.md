# Overview

ProteaAudio is a minimalist stereo audio mixer/playback library for

- Linux
- MacOS
- Windows

Supported audio formats:
- Wav
- Ogg
- Raw linear PCM

## Audio backend flavours

The `proteaaudio` library has two low-level backend implementation.

- [proteaaudio-sdl](./proteaaudio-sdl) is using SDL2
- [proteaaudio](./proteaaudio) is directly using the OS native audio library

 Both library implements the same Haskell API.
