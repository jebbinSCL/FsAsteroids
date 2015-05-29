module GameConfig 

open System

open OpenTK
open OpenTK.Graphics.OpenGL

let loadConfig (game : GameWindow) = 
    game.VSync <- VSyncMode.On
    GL.Enable(EnableCap.Blend)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)

let resizeConfig (game : GameWindow) = 
    GL.Viewport(game.ClientRectangle.X, game.ClientRectangle.Y, game.ClientRectangle.Width, game.ClientRectangle.Height)
    let aspectRatio = float32 game.Width / float32 game.Height
    let fov = Math.PI / 2.
    
    let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 fov, aspectRatio, 1.0f, 100.0f)
    
    GL.MatrixMode(MatrixMode.Projection)
    GL.LoadMatrix(&projection)

let renderConfig() = 
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
    let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
    GL.MatrixMode(MatrixMode.Modelview)
    GL.LoadMatrix(&modelview)
