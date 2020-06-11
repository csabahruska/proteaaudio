#include <cstring>
#include <cstdlib>
#include "proteaaudio_binding.h"

// check if either PROTEAAUDIO_SDL or PROTEAAUDIO_RT is set
#if defined PROTEAAUDIO_SDL && defined PROTEAAUDIO_RT
#error "both PROTEAAUDIO_SDL and PROTEAAUDIO_RT is defined"
#endif

#if !defined PROTEAAUDIO_SDL && !defined PROTEAAUDIO_RT
#error "neither PROTEAAUDIO_SDL and PROTEAAUDIO_RT is defined"
#endif

// import backend api
#ifdef PROTEAAUDIO_RT
#include "proAudioRt.h"
#endif

#ifdef PROTEAAUDIO_SDL
#include "proAudioSdl.h"
#endif

using namespace std;

typedef struct {
  unsigned char * data;
  size_t size;
  size_t pos;
} MEM_FILE;

static size_t mem_fread ( void * ptr, size_t size, size_t count, FILE * stream_ ) {
  MEM_FILE * stream = (MEM_FILE*)stream_;
  if (!ptr || !stream) return 0;
  size_t remain = stream->size - stream->pos;
  if (remain <= 0) return 0;
  size_t cnt = size*count;
  size_t bytes = (cnt>remain)?remain:cnt;
  memcpy(ptr,&stream->data[stream->pos],bytes);
  stream->pos = stream->pos + bytes;
  return cnt;
}

static AudioSample* loadWavFromMemory(unsigned char * ptr, size_t len) {
  MEM_FILE* fp = new MEM_FILE();
  if (!fp) return 0;
  fp->data = ptr;
  fp->size = len;
  fp->pos = 0;
  AudioSample * pSample = AudioSample::readWav((FILE*)fp, mem_fread);
  delete fp;
  return pSample;
}

extern "C" {
extern int stb_vorbis_decode_memory(unsigned char *mem, int len, int *channels, int* sample_rate, short **output);
};

static AudioSample* loadOggFromMemory(unsigned char * file_data_ptr, size_t file_data_len) {
  int channels, sampleRate;
  short *decoded;
  int len = stb_vorbis_decode_memory(file_data_ptr, file_data_len, &channels, &sampleRate, &decoded);
  if(len<0) return 0;
  // convert to AudioSample:
  unsigned int size = len*channels*sizeof(short);
  unsigned char * data = new unsigned char[size];
  if(!data) return 0;
  memcpy(data,decoded, size);
  free(decoded);
  return new AudioSample(data, size, channels, sampleRate, 16);
}



// generic
int initAudio(int nTracks, int frequency, int chunkSize) {
    if (nTracks < 1 || nTracks > (1<<UH_PAYLOAD_BIT_WIDTH) )
        return 0;
#ifdef PROTEAAUDIO_SDL
    DeviceAudio* pAudio = DeviceAudioSdl::create(nTracks, frequency, chunkSize);
#endif
#ifdef PROTEAAUDIO_RT
    DeviceAudio* pAudio = DeviceAudioRt::create(nTracks, frequency, chunkSize);
#endif
    return pAudio != 0;
}

void finishAudio() {
    DeviceAudio::destroy();
}

int loaderAvailable(char* suffix) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.loaderAvailable(suffix);
}

void volume(float left, float right) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return;
    audio.volume(left,right);
}

sample_t sampleFromFile(char* filename, float volume) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.sampleFromFile(filename, volume);
}

sample_t _sampleFromMemoryPcm(char *pcm_data, int size, int channels, int sampleRate, int bitsPerSample, float volume) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    unsigned char * data = new unsigned char[size];
    if(!data) return 0;
    memcpy(data, pcm_data, size);
    AudioSample * pSample = new AudioSample(data, size, channels, sampleRate, bitsPerSample);
    if(!pSample) return 0;
    sample_t ret = audio.sampleFromMemory(*pSample, volume);
    delete pSample;
    return ret;
}
sample_t _sampleFromMemoryWav(char *data, int size, float volume) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    AudioSample* pSample = loadWavFromMemory((unsigned char*)data, size);
    if(!pSample) return 0;
    sample_t ret = audio.sampleFromMemory(*pSample, volume);
    delete pSample;
    return ret;
}

sample_t _sampleFromMemoryOgg(char *data, int size, float volume) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    AudioSample* pSample = loadOggFromMemory((unsigned char*)data, size);
    if(!pSample) return 0;
    sample_t ret = audio.sampleFromMemory(*pSample, volume);
    delete pSample;
    return ret;
}

int soundActiveAll() {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return (int)audio.soundActive();
}

void soundStopAll() {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return;
    audio.soundStop();
}

int sampleDestroy(uint64_t sample) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return false;
    return audio.sampleDestroy(sample);
}

// sound
sound_t soundLoop(sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundLoop(sample, volumeL, volumeR, disparity, pitch);
}

sound_t soundPlay(sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundPlay(sample, volumeL, volumeR, disparity, pitch);
}

sound_t soundLoopOn(unsigned int track, sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundLoopOn(track, sample, volumeL, volumeR, disparity, pitch);    
}

sound_t soundPlayOn(unsigned int track, sample_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundPlayOn(track, sample, volumeL, volumeR, disparity, pitch);    
}

int soundUpdate(sound_t sound, float volumeL, float volumeR, float disparity, float pitch) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundUpdate(sound, volumeL, volumeR, disparity, pitch);
}

int soundStop(sound_t sound) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundStop(sound);
}

int soundActive(sound_t sound) {
    DeviceAudio & audio = DeviceAudio::singleton();
    if((&audio) == 0) return 0;
    return audio.soundActive(sound);
}
