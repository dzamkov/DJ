module DJ.Sound

open System
open System.Collections.Generic
open System.IO
open OpenTK.Audio
open OpenTK.Audio.OpenAL
open Mp3Sharp

let getALFormat format =
    match format with
    | SoundFormat.Pcm16BitMono -> ALFormat.Mono16
    | SoundFormat.Pcm16BitStereo -> ALFormat.Stereo16
    | _ -> ALFormat.Mono16

let getSampleSize format =
    match format with
    | SoundFormat.Pcm16BitMono -> 2
    | SoundFormat.Pcm16BitStereo -> 4
    | _ -> 2

let getSample (buffer : byte[]) index =
    let value = BitConverter.ToInt16 (buffer, index)
    float value / 32768.0

/// A sample buffer that includes a reference to an OpenAL buffer along with the current sample
/// data.
type Buffer (size : int) =
    let data = Array.zeroCreate size
    let id = AL.GenBuffer ()

    /// Gets the OpenAL ID for this buffer.
    member this.ID = id

    /// Gets the byte data for this buffer.
    member this.Data = data

 
type Sound (input : Stream) =
    static let context = new AudioContext ()
    let mutable stream = new Mp3Stream (input)
    let source = AL.GenSource ()
    let mutable playing = false
    let mutable sampleFrequency = 0
    let mutable sampleFormat = SoundFormat.Pcm16BitMono
    let bufferQueue = Queue ()
    let mutable bufferSampleOffset = 0
    let mutable peakMax = 0.0
    let mutable peakMin = 0.0

    /// Dequeues all buffers.
    let clearBuffers () =
        let mutable buffersQueued = 0
        AL.GetSource (source, ALGetSourcei.BuffersQueued, &buffersQueued)
        let buffers = Array.zeroCreate buffersQueued
        AL.SourceUnqueueBuffers (source, buffersQueued, buffers)
        bufferQueue.Clear ()
        bufferSampleOffset <- 0

    /// Queues the given buffer for playing.
    let queueBuffer (buffer : Buffer) =
        AL.BufferData (buffer.ID, getALFormat sampleFormat, buffer.Data, buffer.Data.Length, sampleFrequency)
        AL.SourceQueueBuffer (source, buffer.ID)
        bufferQueue.Enqueue buffer

    /// Populates the given buffer with the next set of data for the sound.
    let writeBuffer (buffer : Buffer) =
        stream.Read (buffer.Data, 0, buffer.Data.Length) |> ignore
        sampleFrequency <- stream.Frequency
        sampleFormat <- stream.Format

    /// Creates, writes and queues the initial buffers for this sound.
    let initializeBuffers count size =
        for i = 0 to count - 1 do
            let buffer = Buffer size
            writeBuffer buffer
            queueBuffer buffer

    do initializeBuffers 4 65536

    /// Gets the amount of samples in the given buffer.
    let bufferSampleSize (buffer : Buffer) = buffer.Data.Length / getSampleSize sampleFormat

    /// Processes the sound information (beat volume) in the given buffer.
    let processSamples (buffer : Buffer) start count =
        let sampleSize = getSampleSize sampleFormat
        let peakSmoothing = 1.0 / float sampleFrequency
        for i = 0 to count - 1 do
            let sample = getSample buffer.Data ((start + i) * sampleSize)
            peakMax <- max sample (peakMax - peakSmoothing)
            peakMin <- min sample (peakMin + peakSmoothing)

    member this.Update () =

        // Refill processed buffers.
        let mutable buffersProcessed = 0
        AL.GetSource (source, ALGetSourcei.BuffersProcessed, &buffersProcessed)
        if buffersProcessed > 0 then
            for i = 0 to buffersProcessed - 1 do
                let buffer = bufferQueue.Dequeue ()
                AL.SourceUnqueueBuffer source |> ignore
                processSamples buffer bufferSampleOffset (bufferSampleSize buffer - bufferSampleOffset)
                bufferSampleOffset <- 0
                writeBuffer buffer
                queueBuffer buffer
            if AL.GetSourceState source <> ALSourceState.Playing && playing then
                AL.SourcePlay source

        // Process additional samples.
        let mutable sampleOffset = 0
        AL.GetSource (source, ALGetSourcei.SampleOffset, &sampleOffset)
        if sampleOffset > 0 then
            let buffer = bufferQueue.Peek ()
            let sampleSize = getSampleSize sampleFormat
            let sampleOffset = min sampleOffset (buffer.Data.Length / sampleSize - 1)
            processSamples buffer bufferSampleOffset (sampleOffset - bufferSampleOffset)
            bufferSampleOffset <- sampleOffset

    member this.Play () =
        AL.SourcePlay source
        playing <- true

    member this.Pause () =
        AL.SourcePause source
        playing <- false

    member this.Reset () =
        AL.SourceStop source
        playing <- false
        input.Seek (0L, SeekOrigin.Begin) |> ignore
        stream <- new Mp3Stream (input)
        clearBuffers ()
        initializeBuffers 4 65536

    /// Gets or sets wether the sound is playing.
    member this.Playing
        with get () = playing
        and set value =
            match playing, value with
            | false, true -> this.Play ()
            | true, false -> this.Pause ()
            | _ -> ()

    /// Gets or sets the playback pitch.
    member this.Pitch 
        with get () =
            let mutable pitch = 0.0f
            AL.GetSource (source, ALSourcef.Pitch, &pitch)
            float pitch
        and set (value : float) = AL.Source (source, ALSourcef.Pitch, float32 value)

    /// Gets or sets the playback volume.
    member this.Volume
        with get () =
            let mutable volume = 0.0f
            AL.GetSource (source, ALSourcef.Gain, &volume)
            float volume
        and set (value : float) = AL.Source (source, ALSourcef.Gain, float32 value)

    /// Gets the current beat volume of the sound as a value between 0.0 and 1.0.
    member this.BeatVolume = (peakMax - peakMin) * 0.5