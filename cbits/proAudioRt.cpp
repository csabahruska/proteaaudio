#include "proAudioRt.h"
#include <cmath>
#include <cstdio>
#include <climits>
#include <cstring>
#include <cstdlib>

#ifdef __RTAUDIO_DUMMY__
#error "No suitable audio backend has found for RtAudio! Install the dev package with headers for the system's audio library and make sure it is in the include path!"
#endif

using namespace std;

struct _AudioTrack {
    /// sample
    AudioSample * sample;

    /// position in sample in frames
    unsigned int dpos;
    /// length of sample in frames
    unsigned int dlen;
    /// disparity in seconds between left and right, normally 0.0f
    float disparity;
    /// left volume
    float volL;
    /// right volume
    float volR;
    /// pitch factor, normally 1.0f
    float pitch;
    /// stores whether sample has to be looped
    bool isLoop;
    /// stores whether sample is currently playing
    bool isPlaying;
    /// unique id for this sound
    uint64_t uniqueId;
};

DeviceAudio* DeviceAudioRt::create(unsigned int nTracks, unsigned int frequency, unsigned int chunkSize) {
    if(!s_instance) {
        DeviceAudioRt* pAudio = new DeviceAudioRt(nTracks,frequency,chunkSize);
        if(!pAudio->m_freqOut) delete pAudio;
        else s_instance = pAudio;
    }
    return s_instance;
}

DeviceAudioRt::DeviceAudioRt(unsigned int nTracks, unsigned int frequency, unsigned int chunkSize) : DeviceAudio(), m_uniqueCounter(0) {
    if ( m_dac.getDeviceCount() < 1 ) {
        fprintf(stderr,"DeviceAudioRt ERROR: No audio devices found!\n");
        return;
    }
    // Set our stream parameters for output only.
    RtAudio::StreamParameters oParams;
    oParams.deviceId = m_dac.getDefaultOutputDevice(); // default device
    oParams.nChannels = 2; // stereo
    oParams.firstChannel = 0;

    // initialize tracks:
    m_nSound=nTracks;
    ma_sound=new _AudioTrack[m_nSound];
    memset(ma_sound,0,m_nSound*sizeof(_AudioTrack));
    m_freqOut = frequency;

    try {
        m_dac.openStream( &oParams, NULL, RTAUDIO_SINT16, frequency, &chunkSize, &cbMix, (void *)this );
        m_dac.startStream();
    }
    catch ( RtAudioError& e ) {
        fprintf(stderr,"%s\n", e.getMessage().c_str());
        if(m_dac.isStreamOpen()) m_dac.closeStream();
        return;
    }

}

DeviceAudioRt::~DeviceAudioRt() {
    if(m_dac.isStreamOpen()) m_dac.closeStream();
    delete [] ma_sound;
    for( map<uint64_t,AudioSample*>::iterator it=mm_sample.begin(); it!=mm_sample.end(); ++it)
        delete it->second;
    mm_sample.clear();
}

uint64_t DeviceAudioRt::sampleFromMemory(const AudioSample & sample, float volume) {
    AudioSample * pSample = new AudioSample(sample);
    if(volume!=1.0f) pSample->volume(volume);
    pSample->bitsPerSample(16);
    mm_sample.insert(make_pair(++m_uniqueCounter,pSample));
    return m_uniqueCounter;
}

bool DeviceAudioRt::sampleDestroy(uint64_t sample) {
    // look for sample:
    map<uint64_t,AudioSample*>::iterator iter=mm_sample.find(sample);
    if( iter == mm_sample.end() ) return false;
    // stop currently playing sounds referring to this sample:
    for (unsigned int i=0; i<m_nSound; ++i ) if(ma_sound[i].sample == iter->second)
        ma_sound[i].isPlaying=false;
    // cleanup:
    delete iter->second;
    mm_sample.erase(iter);
    return true;
}

const AudioSample* DeviceAudioRt::sample(uint64_t handle) const {
    map<uint64_t,AudioSample*>::const_iterator it=mm_sample.find(handle);
    if( it == mm_sample.end() ) return 0;
    return it->second;
}

uint64_t DeviceAudioRt::soundPlay(uint64_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    // look for an empty (or finished) sound track
    unsigned int i;
    for ( i=0; i<m_nSound; ++i )
        if (!ma_sound[i].isPlaying) break;
    return soundPlayOn(i,sample,volumeL,volumeR,disparity,pitch);
}

uint64_t DeviceAudioRt::soundPlayOn(unsigned int i, uint64_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    if ( i >= m_nSound ) return 0; // invalid, or no empty slot found

    // look for sample:
    map<uint64_t,AudioSample*>::iterator iter=mm_sample.find(sample);
    if( iter == mm_sample.end() ) return 0; // no sample found

    uint64_t uniqueHandle = UH_PACK_UNIQUE_HANDLE(++m_uniqueCounter, i);
    unsigned int sampleRate = iter->second->sampleRate();
    if(sampleRate!=m_freqOut) pitch*=(float)sampleRate/(float)m_freqOut;

    // put the sample data in the slot and play it
    ma_sound[i].sample = iter->second;
    ma_sound[i].dlen = iter->second->frames();
    ma_sound[i].dpos = 0;
    ma_sound[i].volL=volumeL;
    ma_sound[i].volR=volumeR;
    ma_sound[i].disparity=disparity;
    ma_sound[i].pitch=fabs(pitch);
    ma_sound[i].isLoop=false;
    ma_sound[i].isPlaying=true;
    ma_sound[i].uniqueId=UH_UNPACK_UNIQUE_ID(uniqueHandle);
    return uniqueHandle;
}

uint64_t DeviceAudioRt::soundLoop(uint64_t sample, float volumeL, float volumeR, float disparity, float pitch ) {
    uint64_t uniqueHandle=soundPlay(sample,volumeL,volumeR,disparity, pitch);
    if(uniqueHandle) ma_sound[UH_UNPACK_PAYLOAD(uniqueHandle)].isLoop=true;
    return uniqueHandle;
}

uint64_t DeviceAudioRt::soundLoopOn(unsigned int i, uint64_t sample, float volumeL, float volumeR, float disparity, float pitch) {
    uint64_t uniqueHandle=soundPlayOn(i,sample,volumeL,volumeR,disparity, pitch);
    if(uniqueHandle) ma_sound[UH_UNPACK_PAYLOAD(uniqueHandle)].isLoop=true;
    return uniqueHandle;
}

bool DeviceAudioRt::soundUpdate(uint64_t uniqueHandle, float volumeL, float volumeR, float disparity, float pitch ) {
    unsigned int sound = UH_UNPACK_PAYLOAD(uniqueHandle);
    if((sound>=m_nSound) || !ma_sound[sound].isPlaying || ma_sound[sound].uniqueId!=UH_UNPACK_UNIQUE_ID(uniqueHandle)) return false;
    ma_sound[sound].volL=volumeL;
    ma_sound[sound].volR=volumeR;
    ma_sound[sound].disparity=disparity;
    unsigned int sampleRate = ma_sound[sound].sample->sampleRate();
    if(sampleRate!=m_freqOut) pitch*=(float)sampleRate/(float)m_freqOut;
    ma_sound[sound].pitch=fabs(pitch);
    return true;
}

bool DeviceAudioRt::soundStop(uint64_t uniqueHandle) {
    unsigned int sound = UH_UNPACK_PAYLOAD(uniqueHandle);
    if((sound>=m_nSound) || !ma_sound[sound].isPlaying || ma_sound[sound].uniqueId!=UH_UNPACK_UNIQUE_ID(uniqueHandle)) return false;
    ma_sound[sound].isPlaying=false;
    return true;
}

void DeviceAudioRt::soundStop() {
    for (unsigned int i=0; i<m_nSound; ++i )
        ma_sound[i].isPlaying=false;
}

unsigned int DeviceAudioRt::soundActive() const {
    if(!const_cast<RtAudio*>(&m_dac)->isStreamRunning() ) return 0;
    unsigned int ret = 0, i;
    for ( i=0; i<m_nSound; ++i )
        if (ma_sound[i].isPlaying) ++ret;
    return ret;
}

bool DeviceAudioRt::soundActive(uint64_t uniqueHandle) {
    unsigned int sound = UH_UNPACK_PAYLOAD(uniqueHandle);
    if((sound>=m_nSound) || ma_sound[sound].uniqueId!=UH_UNPACK_UNIQUE_ID(uniqueHandle)) return false;
    return ma_sound[sound].isPlaying;
}

int DeviceAudioRt::mixOutputFloat(signed short *outputBuffer, unsigned int nFrames) {
    for(unsigned int j=0; j<nFrames; ++j) {
        float left=0.0f;
        float right=0.0f;
        for (unsigned int i=0; i<m_nSound; ++i ) if(ma_sound[i].isPlaying) {
            unsigned int nChannels = ma_sound[i].sample->channels();
            if((ma_sound[i].pitch==1.0f)&&!ma_sound[i].disparity) { // use optimized default mixing:
                unsigned int currPos=ma_sound[i].dpos+j;
                if(ma_sound[i].isLoop) currPos%=ma_sound[i].dlen;
                else if(currPos >= ma_sound[i].dlen) continue;
                currPos*=ma_sound[i].sample->sizeFrame();
                float dataL = (float)(*((signed short *)(&ma_sound[i].sample->data()[currPos])));
                left += dataL * m_volL*ma_sound[i].volL;
                float dataR = (nChannels>1) ? (float)(*((signed short *)(&ma_sound[i].sample->data()[currPos+2]))) : dataL;
                right+= dataR * m_volR*ma_sound[i].volR;
            }
            else { // use nearest sample and disparity:
                double fract=ma_sound[i].dpos+j*ma_sound[i].pitch;
                unsigned int currPos=(unsigned int)fract;
                fract = fmod(fract,1.0);
                int currPosL= (ma_sound[i].disparity<0.0f) ? currPos+int(m_freqOut*ma_sound[i].disparity) : currPos;
                int currPosR= (ma_sound[i].disparity>0.0f) ? currPos-int(m_freqOut*ma_sound[i].disparity) : currPos;
                if(nChannels>1) currPosR+=sizeof(signed short); // use second channel
                if(ma_sound[i].isLoop) {
                    currPosL+=ma_sound[i].dlen;
                    currPosL%=ma_sound[i].dlen;
                    currPosR+=ma_sound[i].dlen;
                    currPosR%=ma_sound[i].dlen;
                }
                if(currPosL<0) {
                    // do nothing
                }
                else if((unsigned int)currPosL+1 < ma_sound[i].dlen) {
                    currPosL*=ma_sound[i].sample->sizeFrame();
                    float dataL = (1.0f-(float)fract)*(float)(*((signed short *)(&ma_sound[i].sample->data()[currPosL])))
                        + (float)fract*(float)(*((signed short *)(&ma_sound[i].sample->data()[currPosL+ma_sound[i].sample->sizeFrame()])));
                    left += dataL * m_volL*ma_sound[i].volL;
                }
                else if((unsigned int)currPosL+1 == ma_sound[i].dlen) {
                    currPosL*=ma_sound[i].sample->sizeFrame();
                    float dataL = (float)(*((signed short *)(&ma_sound[i].sample->data()[currPosL])));
                    left += dataL * m_volL*ma_sound[i].volL;
                }

                if(currPosR<0) {
                    // do nothing
                }
                else if((unsigned int)currPosR+1 < ma_sound[i].dlen) {
                    currPosR*=ma_sound[i].sample->sizeFrame();
                    float dataR = (1.0f-(float)fract)*(float)(*((signed short *)(&ma_sound[i].sample->data()[currPosR])))
                        + (float)fract*(float)(*((signed short *)(&ma_sound[i].sample->data()[currPosR+ma_sound[i].sample->sizeFrame()])));
                    right += dataR * m_volR*ma_sound[i].volR;
                }
                else if((unsigned int)currPosR+1 == ma_sound[i].dlen) {
                    currPosR*=ma_sound[i].sample->sizeFrame();
                    float dataR = (float)(*((signed short *)(&ma_sound[i].sample->data()[currPosR])));
                    right += dataR * m_volR*ma_sound[i].volR;
                }
            }
        }
        // clamp and set output:
        outputBuffer[2*j] = left>SHRT_MAX ? SHRT_MAX : left<SHRT_MIN ? SHRT_MIN : (signed short)left;
        outputBuffer[2*j+1] = right>SHRT_MAX ? SHRT_MAX : right<SHRT_MIN ? SHRT_MIN : (signed short)right;
    }
    // calculate new pos:
    for (unsigned int i=0; i<m_nSound; ++i ) {
        if(ma_sound[i].pitch==1.0f) ma_sound[i].dpos += nFrames;
        else ma_sound[i].dpos += (unsigned int)(nFrames*ma_sound[i].pitch);

        if(ma_sound[i].isLoop) ma_sound[i].dpos%=ma_sound[i].dlen;
        else if(ma_sound[i].dpos>ma_sound[i].dlen+2*abs(int(m_freqOut*-ma_sound[i].disparity)))
            ma_sound[i].isPlaying=false;
    }
    return 0;
}
