#ifndef _PRO_AUDIO
#define _PRO_AUDIO

#include <string>
#include <map>

/** @file proAudio.h
 \brief Public interface of proteaAudio
 
 Contains the declaration of the audio sample class and the abstract base class for audio mixer/playback devices
 
 \author  Gerald Franz, www.viremo.de
 \version 0.6
 
 License notice (zlib license):

 (c) 2009 by Gerald Franz, www.viremo.de

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

//--- class AudioSample --------------------------------------------
/// class representing an audio sample
class AudioSample {
public:
	/// constructor from memory data
	AudioSample(unsigned char * data, unsigned int size, unsigned short channels, unsigned int sampleRate, unsigned short bitsPerSample) :
		m_data(data), m_size(size), m_channels(channels), m_sampleRate(sampleRate), m_bitsPerSample(bitsPerSample) { }
	/// copy constructor
	AudioSample(const AudioSample & source);
	/// destructor
	~AudioSample() { delete[] m_data; }
	
	/// allows accessing sample data
	unsigned char * data() { return m_data; };
	/// allows reading sample data
	const unsigned char * data() const { return m_data; };
	/// returns sample size in bytes
	unsigned int size() const { return m_size; }
	/// returns sample size in number of frames
	unsigned int frames() const { return m_size/m_channels/(m_bitsPerSample>>3); }
	/// returns size of a single frame in bytes
	unsigned int sizeFrame() const { return m_channels*(m_bitsPerSample>>3); }
	/// returns number of parallel channels, 1 mono, 2 stereo
	unsigned short channels() const { return m_channels; }
	/// returns number of frames per second, e.g., 44100, 22050
	unsigned int sampleRate() const { return m_sampleRate; }
	/// returns number of bits per mono sample, e.g., 8, 16
	unsigned short bitsPerSample() const { return m_bitsPerSample; }
	/// converts to a different bit rate, e.g., 8, 16
	bool bitsPerSample(unsigned short bits);
	/// returns number of bytes per sample, e.g., 1, 2
	unsigned short bytesPerSample() const { return m_bitsPerSample>>3; }

	/// changes volume by given factor
	void volume(float f);
	
	/// loads a WAV file
	static AudioSample* loadWav(const std::string & fname);
	/// reads WAV data from a stream via a function compatible to std::fread
	static AudioSample* readWav(FILE* stream, size_t (*readFunc)( void *, size_t, size_t, FILE *));
protected:
	/// stores sample data
	unsigned char * m_data;
	/// sample size in bytes
	unsigned int m_size;
	/// number of parallel channels, 1 mono, 2 stereo
	unsigned short m_channels;
	/// number of samples per second, e.g., 44100, 22050
	unsigned int m_sampleRate;
	/// number of bits per sample, e.g., 8, 16
	unsigned short m_bitsPerSample;
};

//--- class DeviceAudio --------------------------------------------

/// abstract base class for stereo audio mixer/playback devices
class DeviceAudio {
public:
    /// returns singleton object
	/** This call is only allowed after a successful precedent creation of an audio device */
    static DeviceAudio& singleton() { return *s_instance; }
    /// calls the destructor of the singleton object
    static void destroy() { if(s_instance) delete s_instance; s_instance=0; };

	/// sets master volume
    void volume(float left, float right) { m_volL=left; m_volR=right; }
    /// sets master volume
    void volume(float leftAndRight) { m_volL=m_volR=leftAndRight; }
	/// registers an audio sample loader function handling a file type identified by suffix
	/** The function has to be of type AudioSample * loadXYZ(const std::string & filename).*/
	bool loaderRegister(AudioSample *(*loadFunc)(const std::string &), const std::string & suffix);
	/// returns true in case a loader for this file type is available
	bool loaderAvailable(const std::string & suffix) const;

    /// loads a sound sample from file, optionally adjusts volume, returns handle
    virtual unsigned int sampleFromFile(const std::string & filename, float volume=1.0f);
    /// converts a sound sample to internal audio format, returns handle
    virtual unsigned int sampleFromMemory(const AudioSample & sample, float volume=1.0f)=0;
	/// deletes a previously created sound sample resource identified by its handle
	virtual bool sampleDestroy(unsigned int sample)=0;
	/// allows read access to a sample identified by its handle
	virtual const AudioSample* sample(unsigned int handle) const { return 0; }
	
    /// plays a specified sound sample once and sets its parameters
    /** \param sample  handle of a previously loaded sample
     \param volumeL (optional) left volume
     \param volumeR (optional) right volume
     \param disparity (optional) time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right.
     \param pitch (optional) pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample.
     \return a handle to the currently played sound or -1 in case of error */
    virtual unsigned int soundPlay(unsigned int sample, float volumeL=1.0f, float volumeR=1.0f, float disparity=0.0f, float pitch=1.0f )=0;
    /// plays a specified sound sample continuously and sets its parameters
     /** \param sample  handle of a previously loaded sample
     \param volumeL (optional) left volume
     \param volumeR (optional) right volume
     \param disparity (optional) time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right.
     \param pitch (optional) pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample.
     \return a handle to the currently played sound or -1 in case of error */
    virtual unsigned int soundLoop(unsigned int sample, float volumeL=1.0f, float volumeR=1.0f, float disparity=0.0f, float pitch=1.0f )=0;
    /// updates parameters of a specified sound
     /** \param sound  handle of a currently active sound
     \param volumeL left volume
     \param volumeR right volume
     \param disparity (optional) time difference between left and right channel in seconds. Use negative values to specify a delay for the left channel, positive for the right.
     \param pitch (optional) pitch factor for playback. 0.5 corresponds to one octave below, 2.0 to one above the original sample.
     \return true in case the parameters have been updated successfully */
    virtual bool soundUpdate(unsigned int sound, float volumeL, float volumeR, float disparity=0.0f, float pitch=1.0f )=0;
    /// stops a specified sound immediately
    virtual bool soundStop(unsigned int sound)=0;
    /// stops all sounds immediately
    virtual void soundStop()=0;
	/// returns number of currently active sounds
	virtual unsigned int soundActive() const=0;

protected:
	/// constructor
	DeviceAudio();
    /// destructor
    virtual ~DeviceAudio() { s_instance = 0; }
	
	/// stores output stream frequency
	unsigned int m_freqOut;
    /// stores left master volume
    float m_volL;
    /// stores right master volume
    float m_volR;
	/// map associating suffixes to loader functions
	std::map<std::string, AudioSample * (*)(const std::string &)> mm_loader;
	
    /// pointer to singleton
    static DeviceAudio * s_instance;
};

#endif // _PRO_AUDIO
