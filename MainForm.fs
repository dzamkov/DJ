module DJ.MainForm

open DJ.Sound
open System.Windows.Forms
open System.Drawing

type MainForm (sound : Sound, name : string) as this =
    inherit Form ()

    do
        this.Text <- name
        this.Width <- 400
        this.Height <- 400
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.FixedSingle
        this.MaximizeBox <- false

    let width () = this.ClientSize.Width
    let height () = this.ClientSize.Height

    /// Gets the pitch and volume when the play control is at the given point.
    let getPitchVolume (point : Point) =
        let xOffset = 2.0 * (float point.X / float (width ()) - 0.5)
        let yOffset = 2.0 * (float point.Y / float (height ()) - 0.5)
        (1.5 ** xOffset, 0.5 - yOffset * 0.5)

    /// Gets the x coordinate for the given pitch.
    let getPitchX pitch =
        let xOffset = log pitch / log 1.5
        int ((xOffset / 2.0 + 0.5) * float (width ())) 

        /// Gets the y coordinate for the given volume.
    let getVolumeY volume =
        let yOffset = (volume - 0.5) * -2.0
        int ((yOffset / 2.0 + 0.5) * float (height ()))

    let paint (g : Graphics) =
        g.Clear Color.White
        use majorPen = new Pen (Color.LightBlue, 4.0f)
        use minorPen = new Pen (Color.LightGray, 3.0f)
        let divisions = 10
        let width = float32 (width ())
        let height = float32 (height ())
        let xDelta = width / float32 divisions
        let yDelta = height / float32 divisions
        for i = 1 to divisions - 1 do
            g.DrawLine (minorPen, xDelta * float32 i, 0.0f, xDelta * float32 i, height)
            g.DrawLine (minorPen, 0.0f, yDelta * float32 i, width, yDelta * float32 i)
        g.DrawLine (majorPen, width / 2.0f, 0.0f, width / 2.0f, height)
        g.DrawLine (majorPen, 0.0f, height / 2.0f, width, height / 2.0f)

        let beatVolume = sound.BeatVolume
        let beatFrequency = sound.BeatFrequency
        use beatPen = new Pen (Color.LightGreen, 3.0f)
        let beatX = float32 (getPitchX (2.0 / beatFrequency))
        g.DrawLine (beatPen, beatX, 0.0f, beatX, height)

        if sound.Playing then
            let pointX = float32 (getPitchX sound.Pitch)
            let pointY = float32 (getVolumeY sound.Volume)
            g.FillEllipse (Brushes.Red, pointX - 5.0f, pointY - 5.0f, 10.0f, 10.0f)

            let beatPhase = sound.BeatPhase
            use beatPen = new Pen (Color.FromArgb (int ((1.0 - beatPhase) * beatVolume * 255.0), 255, 0, 0), 4.0f)
            let radius = 100.0f * float32 (1.0 - beatPhase)
            g.DrawEllipse (beatPen, pointX - radius * 0.5f, pointY - radius * 0.5f, radius, radius)

    /// Updates this form.
    member this.Update () =
        this.Refresh ()

    override this.OnPaint args =
        paint args.Graphics

    override this.OnMouseDown args =
        this.Refresh ()
        if args.Button = MouseButtons.Left then
            sound.Playing <- true
            let pitch, volume = getPitchVolume args.Location
            sound.Pitch <- pitch
            sound.Volume <- volume
        elif args.Button = MouseButtons.Right then
            sound.Pause ()
        elif args.Button = MouseButtons.Middle then
            sound.Reset ()

    override this.OnMouseMove args =
        this.Refresh ()
        if args.Button = MouseButtons.Left then
            let pitch, volume = getPitchVolume args.Location
            sound.Pitch <- pitch
            sound.Volume <- volume