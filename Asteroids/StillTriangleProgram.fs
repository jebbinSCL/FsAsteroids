open System
open System.Drawing
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

type Game() =
    /// <summary>Creates a 800x600 window with the specified title.</summary>
    inherit GameWindow(800, 600, GraphicsMode.Default, "F# OpenTK Sample")

    do base.VSync <- VSyncMode.On

    let random = new Random()

     /// <summary>Load resources here.</summary>
     /// <param name="e">Not used.</param>
    override o.OnLoad e =
       base.OnLoad(e)
       GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)
       GL.Enable(EnableCap.DepthTest)

     /// <summary>
     /// Called when your window is resized. Set your viewport here. It is also
     /// a good place to set up your projection matrix (which probably changes
     /// along when the aspect ratio of your window).
     /// </summary>
     /// <param name="e">Not used.</param>
    override o.OnResize e =
         base.OnResize e
         GL.Viewport(base.ClientRectangle.X, base.ClientRectangle.Y, base.ClientRectangle.Width, base.ClientRectangle.Height)
         let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.), float32 base.Width / float32 base.Height, 1.f, 64.f)
         GL.MatrixMode(MatrixMode.Projection)
         GL.LoadMatrix(&projection)


     /// <summary>
     /// Called when it is time to setup the next frame. Add you game logic here.
     /// </summary>
     /// <param name="e">Contains timing information for framerate independent logic.</param>
    override o.OnUpdateFrame e =
       base.OnUpdateFrame e
       if base.Keyboard.[Key.Escape] then base.Close()
       if base.Keyboard.[Key.Down] then GL.ClearColor(float32 <| random.NextDouble(), float32 <| random.NextDouble(), float32 <| random.NextDouble(), 0.0f)

     /// <summary>
     /// Called when it is time to render the next frame. Add your rendering code here.
     /// </summary>
     /// <param name="e">Contains timing information.</param>
    override o.OnRenderFrame(e) =
       base.OnRenderFrame e
       GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
       let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
       GL.MatrixMode(MatrixMode.Modelview)
       GL.LoadMatrix(&modelview)

       PrimitiveType.Triangles |> GL.Begin
       GL.Color3(1.f, 1.f, 0.f); GL.Vertex3(-1.f, -1.f, 4.f)
       GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(1.f, -1.f, 4.f)
       GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(0.f, 1.f, 4.f)
       GL.End()

       base.SwapBuffers()

/// <summary>
/// The main entry point for the application.
/// </summary>
let game = new Game()
do game.Run(60.0)