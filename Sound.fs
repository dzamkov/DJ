module DJ.Sound

open System
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

let getSamples format (buffer : byte[]) =
    let sampleSize = getSampleSize format
    let samples = Array.zeroCreate (buffer.Length / sampleSize)
    for i = 0 to samples.Length - 1 do
        samples.[i] <- getSample buffer (i * sampleSize)
    samples
 
type Sound (input : Stream) =
    static let context = new AudioContext ()
    let mutable stream = new Mp3Stream (input)
    let source = AL.GenSource ()
    let bufferSize = 65536
    let buffer = Array.zeroCreate bufferSize
    let mutable playing = false
    let mutable beatPhase = 0.0
    let mutable beatVolume = 0.0
    let mutable beatFrequency = 2.0
    let mutable sampleFormat = SoundFormat.Pcm16BitMono
    let mutable sampleFrequency = 0
    let mutable peakMax = 0.0
    let mutable peakMin = 0.0

    let writeBuffer index =
        stream.Read (buffer, 0, buffer.Length) |> ignore
        let format = stream.Format
        let frequency = stream.Frequency
        sampleFormat <- format
        sampleFrequency <- frequency

        AL.BufferData (index, getALFormat stream.Format, buffer, buffer.Length, frequency)
        AL.SourceQueueBuffer (source, index)

        // Perform beat analysis
        let mutable peakVolume = peakMax - peakMin
        let peakSmoothing = 1.0 / float sampleFrequency
        let beatSmoothing = 0.1 / float sampleFrequency
        for sample in getSamples format buffer do
            let nPeakMax = max sample (peakMax - peakSmoothing)
            let nPeakMin = min sample (peakMin + peakSmoothing)
            let nPeakVolume = nPeakMax - nPeakMin
            let diff = nPeakVolume - peakVolume
            if diff > 0.0 then 
                let beatOffset = (beatPhase + 0.5) % 1.0 - 0.5
                let correction = (1.0 - beatOffset * beatOffset * 4.0) * diff / (beatVolume + 0.01)
                beatPhase <- beatPhase - beatOffset * correction
                beatFrequency <- beatFrequency / (1.0 + beatOffset) ** (correction * 0.05)
            else
                beatPhase <- (beatPhase + (beatFrequency / float sampleFrequency)) % 1.0
            beatVolume <- max (nPeakVolume * 0.5) (beatVolume - beatSmoothing)
            peakMax <- nPeakMax
            peakMin <- nPeakMin
            peakVolume <- nPeakVolume

        ()

    let initializeBuffers count =
        for i = 0 to count - 1 do
            writeBuffer (AL.GenBuffer ())

    do initializeBuffers 4

    member this.Update () =
        let mutable buffersProcessed = 0
        AL.GetSource (source, ALGetSourcei.BuffersProcessed, &buffersProcessed)
        if buffersProcessed > 0 then
            let buffers = Array.zeroCreate buffersProcessed
            AL.SourceUnqueueBuffers (source, buffersProcessed, buffers)
            for i = 0 to buffersProcessed - 1 do
                writeBuffer buffers.[i]
            if AL.GetSourceState source <> ALSourceState.Playing && playing then
                AL.SourcePlay source

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

        let mutable buffersQueued = 0
        AL.GetSource (source, ALGetSourcei.BuffersQueued, &buffersQueued)
        let buffers = Array.zeroCreate buffersQueued
        AL.SourceUnqueueBuffers (source, buffersQueued, buffers)
        for i = 0 to buffersQueued - 1 do
            writeBuffer buffers.[i]
        ()

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

    /// Gets the current beat phase for the sound as a value between 0.0 (beginning of a beat) and
    /// 1.0 (beginning of the next beat).
    member this.BeatPhase =
        let mutable sampleOffset = 0
        let mutable bufferCount = 0
        AL.GetSource (source, ALGetSourcei.SampleOffset, &sampleOffset)
        AL.GetSource (source, ALGetSourcei.BuffersQueued, &bufferCount)
        let sampleOffset = bufferCount * bufferSize / getSampleSize sampleFormat - sampleOffset
        let timeOffset = float sampleOffset / float sampleFrequency
        let beatOffset = timeOffset * beatFrequency
        (beatPhase - beatOffset % 1.0 + 1.0) % 1.0

    /// Gets the current beat volume as a value between 0.0 and 1.0.
    member this.BeatVolume = beatVolume

    /// Gets the current beat frequency in beats per second.
    member this.BeatFrequency = beatFrequency