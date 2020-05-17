#ifdef __cplusplus
extern "C" {
#endif

typedef int sample_t;
typedef int sound_t;

int initAudio(int nTracks, int frequency, int chunkSize);
void finishAudio();
int loaderAvailable(char* suffix);
void volume(float left, float right);
sample_t _sampleFromMemoryPcm(char *data, int size, int channels, int sampleRate, int bitsPerSample, float volume);
sample_t _sampleFromMemoryWav(char *data, int size, float volume);
sample_t _sampleFromMemoryOgg(char *data, int size, float volume);
sample_t sampleFromFile(char* filename, float volume);
int soundActive();
void soundStopAll();
sound_t soundLoop(sample_t sample, float volumeL, float volumeR, float disparity, float pitch);
sound_t soundPlay(sample_t sample, float volumeL, float volumeR, float disparity, float pitch);
int soundUpdate(sound_t sound, float volumeL, float volumeR, float disparity, float pitch);
int soundStop(sound_t sound);
#ifdef __cplusplus
}
#endif
