#include "proAudio.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <climits>

using namespace std;

//--- class AudioSample --------------------------------------------
AudioSample::AudioSample(const AudioSample & source) :
	m_size(source.m_size), m_channels(source.m_channels), m_sampleRate(source.m_sampleRate), m_bitsPerSample(source.m_bitsPerSample) { 
	m_data = new unsigned char [m_size]; memcpy(m_data,source.m_data, m_size); 
}

bool AudioSample::bitsPerSample(unsigned short bits) {
	if(bits==16) {
		if(m_bitsPerSample==8) {
			unsigned char* data = new unsigned char[2*m_size];
			 for(unsigned int i=0; i<m_size; ++i) {
				 signed short *ptr =(signed short*)data+i;
				 *ptr = m_data[i]*255;
			 }
			 delete [] m_data;
			 m_data = data;
			 m_size*=2;
			 return true;
		}
		else if(m_bitsPerSample==16) {
			return true; // nothing to do
		}
		else if(m_bitsPerSample==32) { // float, normalized from -1.0f to 1.0f
			unsigned char* data = new unsigned char[m_size/2];
			 for(unsigned int i=0; i<m_size/4; ++i) {
				 signed short *ptr =(signed short*)data+i;
				 float* src=(float*)m_data+i;
				 *ptr =(signed short)(*src*SHRT_MAX);
			 }
			 delete [] m_data;
			 m_data = data;
			 m_size/=2;
			 return true;
		}
	}
	fprintf(stderr,"AudioSample::bitsPerSample ERROR: conversion from %i to %i bits not supported.\n", m_bitsPerSample, bits);
	return false;
}

void AudioSample::volume(float f) {
	if(m_bitsPerSample==8) for(signed char *ptr =(signed char *)m_data; ptr<(signed char *)m_data+m_size; ++ptr) {
		float value=*ptr * f;
		if(value>CHAR_MAX) *ptr =CHAR_MAX;
		else if(value<CHAR_MIN) *ptr =CHAR_MIN;
		else *ptr =(signed char)value;
	}
	else if(m_bitsPerSample==16) for(signed short *ptr =(signed short*)m_data; ptr<(signed short*)m_data+m_size/2; ++ptr) {
		float value=*ptr * f;
		if(value>SHRT_MAX) *ptr =SHRT_MAX;
		else if(value<SHRT_MIN) *ptr =SHRT_MIN;
		else *ptr =(signed short)value;
	}
	else if(m_bitsPerSample==32) for(float *ptr =(float*)m_data; ptr<(float*)m_data+m_size/4; ++ptr) {
		*ptr *= f;
		if(*ptr>1.0f) *ptr=1.0f;			
		else if(*ptr<-1.0f) *ptr=-1.0f;
	}
	else fprintf(stderr,"AudioSample::changeVolume ERROR: %i bits per sample not supported.\n",m_bitsPerSample);
}

AudioSample* AudioSample::readWav(FILE* stream, size_t (*readFunc)( void *, size_t, size_t, FILE *)) {
	char id[4]; //four unsigned chars to hold chunk IDs
	readFunc(id,sizeof(unsigned char),4,stream);
	if (strncmp(id,"RIFF",4)!=0) return 0;

	unsigned int size;
	readFunc(&size,sizeof(unsigned int),1,stream);

	readFunc(id,sizeof(unsigned char),4,stream);
	if (strncmp(id,"WAVE",4)!=0) return 0;

	unsigned short encoding, block_align, channels, bitsPerSample;
	unsigned int chunk_length, byte_rate, sampleRate;

	readFunc(id, sizeof(unsigned char), 4, stream); //read ID 'fmt ';
	readFunc(&chunk_length, sizeof(unsigned int),1,stream); // header length, 16 expected			
	readFunc(&encoding, sizeof(short), 1, stream); // should be "1" for simple PCM data
	if(encoding!=1) return 0;

	readFunc(&channels, sizeof(short),1,stream);
	readFunc(&sampleRate, sizeof(unsigned int), 1, stream);
	readFunc(&byte_rate, sizeof(unsigned int), 1, stream);
	readFunc(&block_align, sizeof(short), 1, stream);
	readFunc(&bitsPerSample, sizeof(short), 1, stream);
	
	readFunc(id, sizeof(unsigned char), 4, stream); // read ID 'data'
	readFunc(&size, sizeof(unsigned int), 1, stream);
	unsigned char *data = new unsigned char[size];
	readFunc(data, sizeof(unsigned char), size, stream);
	
	return new AudioSample(data,size, channels, sampleRate, bitsPerSample);
}

AudioSample* AudioSample::loadWav(const std::string & fname) {
#ifdef _MSC_VER
	FILE *fp = 0;
	fopen_s(&fp, fname.c_str(), "rb");
#else
	FILE *fp = fopen(fname.c_str(), "rb");
#endif
	if (!fp) return 0;
	AudioSample * pSample = readWav(fp, fread);
	fclose(fp);
	return pSample;
}

//--- class DeviceAudio --------------------------------------------

DeviceAudio* DeviceAudio::s_instance=0;

extern "C" {
extern int stb_vorbis_decode_filename(char *filename, int *channels, int* sample_rate, short **output);
};

static AudioSample* loadOgg(const std::string & fname) {
	int channels, sampleRate;
	short *decoded;
	int len = stb_vorbis_decode_filename(const_cast<char*>(fname.c_str()), &channels, &sampleRate, &decoded);
	if(len<0) return 0;
	// convert to AudioSample:
	unsigned int size = len*channels*sizeof(short);
	unsigned char * data = new unsigned char[size];
	if(!data) return 0;
	memcpy(data,decoded, size);
	free(decoded);
	return new AudioSample(data, size, channels, sampleRate, 16);
}

static string toLower(const string & s) {
    string retStr(s);
    for(size_t i=0; i<s.size(); ++i)
        retStr[i]=static_cast<char>(tolower(retStr[i]));
    return retStr;
}

DeviceAudio::DeviceAudio() : m_freqOut(0), m_volL(1.0f), m_volR(1.0f) {
	loaderRegister(AudioSample::loadWav,"wav");
	loaderRegister(loadOgg,"ogg");
}

unsigned int DeviceAudio::sampleFromFile(const std::string & filename, float volume) { 
	if(filename.rfind('.')>filename.size()) return 0;
	string suffix=toLower(filename.substr(filename.rfind('.')+1));
	map<string, AudioSample * (*)(const string &)>::iterator it = mm_loader.find(suffix);
	if(it==mm_loader.end()) return 0;
	AudioSample* pSample = (*(it->second))(filename);
	if(!pSample) return 0;
	unsigned int ret = sampleFromMemory(*pSample, volume);
	delete pSample;
	return ret; 
}

bool DeviceAudio::loaderRegister(AudioSample *(*loadFunc)(const std::string &), const std::string & suffix) {
	return mm_loader.insert(std::make_pair(toLower(suffix),loadFunc)).second;
}

bool DeviceAudio::loaderAvailable(const std::string & suffix) const {
	return mm_loader.find(toLower(suffix))!=mm_loader.end();
}
