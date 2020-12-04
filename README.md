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

The proteaudio library has two low-level backend implementation.  
Both libraries implement the same Haskell API and they share most of the code to ensure the compatibility.

- [proteaaudio-sdl](./proteaaudio-sdl) is using SDL2
- [proteaaudio](./proteaaudio) is directly using the OS native audio library

