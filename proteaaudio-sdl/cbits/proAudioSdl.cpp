#include "proAudioSdl.h"
extern "C" {
#include <SDL.h>
};

#include <map>
#include <cmath>
#include <climits>

using namespace std;

//--- class DeviceAudioSdl ------------------------------------------

/// internal class to store sample data
class _AudioTrack {
public:
    /// default constructor
    _AudioTrack(Uint8 * pData=0, unsigned int length=0) : data(pData), dlen(length) {
        dpos=0; disparity=0.0f; volL=1.0f; volR=1.0f; pitch=1.0f; isLoop=false; isPlaying=false; };
    /// pointer to raw sample data
    Uint8 *data;
    /// position in playback
    Uint32 dpos;
    /// length of sample in bytes
    Uint32 dlen;
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
};

DeviceAudio* DeviceAudioSdl::create(unsigned int nTracks, unsigned int frequency, unsigned int chunkSize) {
    if(!s_instance) {
		DeviceAudioSdl* pAudio = new DeviceAudioSdl(nTracks,frequency,chunkSize);
		if(!pAudio->m_freqOut) delete pAudio;
		else s_instance = pAudio;
	}
    return s_instance;
}

DeviceAudioSdl::DeviceAudioSdl(unsigned int nTracks, unsigned int frequency, unsigned int chunkSize) : DeviceAudio(), m_sampleCounter(0) {
    // initialize SDL sound system:
	if(!SDL_WasInit(SDL_INIT_AUDIO)&&(SDL_InitSubSystem(SDL_INIT_AUDIO)<0)) {
		fprintf(stderr, "DeviceAudioSdl ERROR: cannot initialize SDL audio subsystem.\n");
		return;
	}

    // set the audio format:
    SDL_AudioSpec desired;
    desired.freq     = frequency;
    desired.format   = AUDIO_S16;
    desired.channels = 2;    // 1 = mono, 2 = stereo
    desired.samples  = chunkSize;  // good low-latency value for callback
    desired.callback = cbOutput;
    desired.userdata = NULL;
    // open the audio device
    if ( SDL_OpenAudio(&desired, &m_spec) < 0 ) {
        fprintf(stderr, "DeviceAudioSdl ERROR: Couldn't open audio.\n");
        return;
    }
    if((m_spec.format!=AUDIO_S16)||(m_spec.channels!=2)) {
        fprintf(stderr, "DeviceAudioSdl WARNING: Could not get desired signed 16 bit stereo. Expect low quality sound.\n");
        m_isDesiredFormat=false;
    }
    else {
        m_isDesiredFormat=true;
    }

    // initialize tracks:
    m_nSound=nTracks;
    ma_sound=new _AudioTrack[m_nSound];

    SDL_PauseAudio(0); // start sound
	m_freqOut = frequency;
}

DeviceAudioSdl::~DeviceAudioSdl() {
    SDL_PauseAudio(1);
    SDL_CloseAudio();
    delete [] ma_sound;
}

void DeviceAudioSdl::cbOutput(void * userData, Uint8 *stream, int len) {
    if(dynamic_cast<DeviceAudioSdl*>(s_instance)->m_isDesiredFormat)
        dynamic_cast<DeviceAudioSdl*>(s_instance)->mixOutputFloat((signed short *)stream, len/2);
    else dynamic_cast<DeviceAudioSdl*>(s_instance)->mixOutputSInt(stream, len);
}

void DeviceAudioSdl::mixOutputFloat(signed short *outputBuffer, unsigned int nFrames) {
    for(unsigned int j=0; j<nFrames; j+=2) {
        float left=0.0f;
        float right=0.0f;
        for (unsigned int i=0; i<m_nSound; ++i ) if(ma_sound[i].isPlaying) {
            if((ma_sound[i].pitch==1.0f)&&!ma_sound[i].disparity) { // use optimized default mixing:
                unsigned int currPos=ma_sound[i].dpos+j;
                if(ma_sound[i].isLoop) currPos%=ma_sound[i].dlen;
                else if(currPos >= ma_sound[i].dlen) continue;
                left +=float(*((Sint16 *)(&ma_sound[i].data[currPos])))
                    *m_volL*ma_sound[i].volL;
                right+=float(*((Sint16 *)(&ma_sound[i].data[currPos])))
                    *m_volR*ma_sound[i].volR;
            }
            else { // use linear interpolation and disparity:
                double fract=ma_sound[i].dpos+j*ma_sound[i].pitch;
                int currPos=int(fract*0.5f)*2;
                fract=(fract-currPos)*0.5f;

                int currPosL= (ma_sound[i].disparity<0.0f)
                    ? currPos-2*int(m_spec.freq*-ma_sound[i].disparity)
                    : currPos;
                int currPosR= (ma_sound[i].disparity>0.0f)
                    ? currPos-2*int(m_spec.freq*ma_sound[i].disparity)
                    : currPos;

                if(ma_sound[i].isLoop) {
                    currPosL=(currPosL+10*ma_sound[i].dlen-2)%(ma_sound[i].dlen-2);
                    currPosR=(currPosR+10*ma_sound[i].dlen-2)%(ma_sound[i].dlen-2);
                }
                if((currPosL < int(ma_sound[i].dlen)-2)&&(currPosL>=0)) {
                    float currWavL=(1.0f-fract)*float(*((Sint16 *)(&ma_sound[i].data[currPosL])))
                        +fract*float(*((Sint16 *)(&ma_sound[i].data[currPosL+2])));
                    left +=currWavL*m_volL*ma_sound[i].volL;
                }
                if((currPosR < int(ma_sound[i].dlen)-2)&&(currPosR>=0)) {
                    float currWavR=(1.0f-fract)*float(*((Sint16 *)(&ma_sound[i].data[currPosR])))
                        +fract*float(*((Sint16 *)(&ma_sound[i].data[currPosR+2])));
                    right+=currWavR*m_volR*ma_sound[i].volR;
                }
            }
        }
        // clamp:
        if(left>(float)SHRT_MAX) outputBuffer[j]=SHRT_MAX;
        else if(left<(float)SHRT_MIN) outputBuffer[j]=SHRT_MIN;
        else outputBuffer[j]=(Sint16)left;
        if(right>(float)SHRT_MAX) outputBuffer[j+1]=SHRT_MAX;
        else if(right<(float)SHRT_MIN) outputBuffer[j+1]=SHRT_MIN;
        else outputBuffer[j+1]=(Sint16)right;
    }
    for (unsigned int i=0; i<m_nSound; ++i ) {
        if(ma_sound[i].pitch==1.0f) ma_sound[i].dpos += nFrames;
        else ma_sound[i].dpos += (unsigned int)(nFrames*ma_sound[i].pitch);

        if(ma_sound[i].isLoop) ma_sound[i].dpos%=ma_sound[i].dlen;
        else if(ma_sound[i].dpos>ma_sound[i].dlen+2*abs(int(m_spec.freq*-ma_sound[i].disparity)))
            ma_sound[i].isPlaying=false;
    }
}

void DeviceAudioSdl::mixOutputSInt(Uint8 *stream, int len) {
    for (unsigned int i=0; i<m_nSound; ++i ) {
        unsigned int amount = ma_sound[i].dlen-ma_sound[i].dpos;
        if (amount > (unsigned int)len) amount = (unsigned int)len;
        SDL_MixAudio(stream, &ma_sound[i].data[ma_sound[i].dpos], amount, SDL_MIX_MAXVOLUME);
        ma_sound[i].dpos += amount;
    }
}

static void adjustVolume(signed short* data, size_t len, float volume) {
	for(size_t i=0; i<len; ++i) {
		signed short & shortValue=data[i];
		float value=shortValue*volume;
		if(value>(float)SHRT_MAX) value=(float)SHRT_MAX; // clamp
		else if(value<(float)SHRT_MIN) value=(float)SHRT_MIN;
		shortValue=(signed short)value;
	}
}

unsigned int DeviceAudioSdl::sampleFromMemory(const AudioSample & sample, float volume) {
    Uint8 destChannels= m_isDesiredFormat ? 1 : m_spec.channels; // convert to mono
	Uint16 format = sample.bytesPerSample()==2 ? AUDIO_S16 :  sample.bytesPerSample()==1 ? AUDIO_S8 : 0;
	if(!format) {
        fprintf(stderr, "DeviceAudioSdl WARNING: %i bit samples not supported.\n", sample.bitsPerSample());
        return 0;
	}
    SDL_AudioCVT cvt;
    SDL_BuildAudioCVT(&cvt, format, sample.channels(), sample.sampleRate(),
                            m_spec.format, destChannels, m_spec.freq);
    cvt.buf = new Uint8[sample.size()*cvt.len_mult];
    memcpy(cvt.buf, sample.data(), sample.size());
    cvt.len = sample.size();
    SDL_ConvertAudio(&cvt);

    _AudioTrack track;
    track.data = cvt.buf;
    track.dlen = cvt.len_cvt;
    track.dpos = 0;
    if (!track.dlen) {
        fprintf(stderr, "DeviceAudioSdl WARNING: Sample has zero length.\n");
        return 0;
    }
    // adjust volume:
    if((volume!=1.0f)&&m_isDesiredFormat)
		adjustVolume((signed short *)track.data, track.dlen/2, volume);

    mm_sample.insert(make_pair(++m_sampleCounter,track));
    return m_sampleCounter;
}

bool DeviceAudioSdl::sampleDestroy(unsigned int sample) {
    // look for sample:
    map<unsigned int,_AudioTrack>::iterator iter=mm_sample.find(sample);
    if( iter == mm_sample.end() ) return false;
	// stop currently playing sounds referring to this sample:
    SDL_LockAudio();
	for (unsigned int i=0; i<m_nSound; ++i ) if(ma_sound[i].data == iter->second.data)
		ma_sound[i].isPlaying=false;
    SDL_UnlockAudio();
	// cleanup:
	delete iter->second.data;
	if(iter->first==m_sampleCounter) --m_sampleCounter;
	mm_sample.erase(iter);
	return true;
}

unsigned int DeviceAudioSdl::soundPlay(unsigned int sample, float volumeL, float volumeR, float disparity, float pitch) {
    // look for sample:
    map<unsigned int,_AudioTrack>::iterator iter=mm_sample.find(sample);
    if(iter==mm_sample.end()) return 0; // no sample found
    // look for an empty (or finished) sound track
    unsigned int i;
    for ( i=0; i<m_nSound; ++i )
        if (!ma_sound[i].isPlaying) break;
    if ( i == m_nSound ) return 0; // no empty slot found

    // put the sample data in the slot and play it
    SDL_LockAudio();
    ma_sound[i].data = iter->second.data;
    ma_sound[i].dlen = iter->second.dlen;
    ma_sound[i].dpos = 0;
    ma_sound[i].volL=volumeL;
    ma_sound[i].volR=volumeR;
    ma_sound[i].disparity=disparity;
    ma_sound[i].pitch=fabs(pitch);
    ma_sound[i].isLoop=false;
    ma_sound[i].isPlaying=true;
    SDL_UnlockAudio();
    return i+1;
}

unsigned int DeviceAudioSdl::soundLoop(unsigned int sample, float volumeL, float volumeR, float disparity, float pitch) {
    unsigned int ret=soundPlay(sample,volumeL,volumeR,disparity, pitch);
    if(ret) {
        SDL_LockAudio();
        ma_sound[ret-1].isLoop=true;
        SDL_UnlockAudio();
    }
    return ret;
}

bool DeviceAudioSdl::soundUpdate(unsigned int sound, float volumeL, float volumeR, float disparity, float pitch ) {
    if(!sound || (sound>m_nSound) || !ma_sound[sound-1].isPlaying) return false;
    SDL_LockAudio();
    ma_sound[--sound].volL=volumeL;
    ma_sound[sound].volR=volumeR;
    ma_sound[sound].disparity=disparity;
    ma_sound[sound].pitch=fabs(pitch);
    SDL_UnlockAudio();
    return true;
}

bool DeviceAudioSdl::soundStop(unsigned int sound) {
    if(!sound||(sound>m_nSound)||!ma_sound[sound-1].isPlaying) return false;
    SDL_LockAudio();
    ma_sound[sound-1].isPlaying=false;
    SDL_UnlockAudio();
    return true;
}

void DeviceAudioSdl::soundStop() {
    SDL_LockAudio();
	for (unsigned int i=0; i<m_nSound; ++i )
		ma_sound[i].isPlaying=false;
    SDL_UnlockAudio();
}

unsigned int DeviceAudioSdl::soundActive() const {
    unsigned int ret = 0, i;
    for ( i=0; i<m_nSound; ++i )
        if (ma_sound[i].isPlaying) ++ret;
    return ret;
}
