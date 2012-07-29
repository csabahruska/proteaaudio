#include "proteaaudio_binding.h"
#include "proAudioRt.h"

// generic
int initAudio(int nTracks, int frequency, int chunkSize) {
    DeviceAudio* pAudio = DeviceAudioRt::create(nTracks, frequency, chunkSize);
    return pAudio != 0;
}

void finishAudio() {
    DeviceAudio::destroy();
}

int loaderAvailable(char* suffix) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return 0;
    return audio.loaderAvailable(suffix);
}

void volume(float left, float right) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return;
    audio.volume(left,right);
}

sample_t sampleFromFile(char* filename, float volume) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return 0;
    return (int)audio.sampleFromFile(filename, volume);
}

int soundActive() {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return 0;
    return (int)audio.soundActive();
}

void soundStopAll() {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return;
    audio.soundStop();
}

// sound
void soundLoop(sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return;
    audio.soundLoop(sample, volumeL,volumeR,disparity,pitch);
}

void soundPlay(sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return;
    audio.soundPlay(sample, volumeL,volumeR,disparity,pitch);
}

int soundUpdate(sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return 0;
    return audio.soundUpdate(sample, volumeL,volumeR,disparity,pitch);
}

int soundStop(sample_t sample) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if(!&audio) return 0;
    return audio.soundStop(sample);
}
