module DJ.Program

open DJ.Sound
open DJ.MainForm
open System
open System.Threading
open System.Windows.Forms
open System.IO

let getSongName (file : string) =
    let parts = file.Split [| '/'; '\\' |]
    let file = parts.[parts.Length - 1]
    let parts = file.Split [| '.' |]
    let file = parts.[0]
    file

let run (file : string) =
    let name = getSongName file
    let file = File.OpenRead file
    let sound = Sound file
    let form = new MainForm (sound, name)
    form.Show ()
    form.Activate ()
    while form.Visible do
        sound.Update ()
        form.Update ()
        Thread.Sleep 5
        Application.DoEvents ()

[<STAThread>]
[<EntryPoint>]
let main (args : string[]) =
    if args.Length > 0 then
        run args.[0]
    else
        use ofd = new OpenFileDialog ()
        ofd.Filter <- "Mp3 files|*.mp3"
        if ofd.ShowDialog () = DialogResult.OK then
            run ofd.FileName
    0