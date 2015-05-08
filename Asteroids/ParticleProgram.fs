
open System

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

// A few useful definitions

[<Measure>] type m
[<Measure>] type s

let diffDate (d1: DateTime) d2 =
   let d = (d2 - d1).Ticks
   float d / float TimeSpan.TicksPerSecond * 1.<s>

let inline dist (x1,y1) (x2,y2) =
   sqrt ((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2))

let rnd = System.Random()
let rndf() = rnd.NextDouble() * 2. - 1.

// Particles system

let lifeSpan = 6.<s>

let mutable generatorX = 0.5<m>
let mutable generatorY = 0.5<m>

type Particle(vx, vy) =
   let mutable x = generatorX
   let mutable y = generatorY
   let birthDate = DateTime.Now

   member p.X = float x
   member p.Y = float y
   member p.Age = diffDate birthDate DateTime.Now
   member p.Move(dt:float<s>) =
       x <- x + vx * dt
       y <- y + vy * dt

// The list of "living" particles
let mutable particles : Particle list = []

//      OpenGL stuff

// display a particle
let display (p: Particle) =
   GL.PushMatrix()
   GL.Translate(p.X - 0.5, 0.8*p.Y - 0.4, 1.)
   let alpha = 1. - p.Age / lifeSpan
   GL.PointSize(40.f)
   GL.Begin(BeginMode.Points)
   GL.Color4(0.06, 0.03, 0.02, alpha)
   GL.Vertex3(0., 0., 0.)
   GL.End()
   GL.PopMatrix()

type Game() =
    inherit GameWindow(800, 600, GraphicsMode.Default, "F# OpenGL demo")
    let mutable lastUpdate = DateTime.Now

    do base.VSync <- VSyncMode.On

    override o.OnLoad(e) =
        base.OnLoad(e)
        GL.ClearColor(0.1f, 0.1f, 0.1f, 0.0f)
        GL.Enable(EnableCap.Blend)
        GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
        GL.Enable(EnableCap.PointSprite)

    override o.OnResize e =
        base.OnResize e
        let rect = base.ClientRectangle
        GL.Viewport(rect.X, rect.Y, rect.Width, rect.Height)
        let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.), float32 base.Width / float32 base.Height, 0.001f, 5.0f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)

    override o.OnUpdateFrame(e) =
        base.OnUpdateFrame(e)
        if base.Keyboard.[Key.Escape] then base.Close()
        if base.Keyboard.[Key.Left] then  generatorX <- generatorX + 0.01<m>
        if base.Keyboard.[Key.Right] then generatorX <- generatorX - 0.01<m>
        if base.Keyboard.[Key.Down] then  generatorY <- generatorY - 0.01<m>
        if base.Keyboard.[Key.Up] then    generatorY <- generatorY + 0.01<m>


    override o.OnRenderFrame(e) =
        base.OnRenderFrame(e)
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
        GL.MatrixMode(MatrixMode.Modelview)

        GL.LoadMatrix(&modelview)
        let t = diffDate lastUpdate DateTime.Now
        lastUpdate <- DateTime.Now
        for p in particles do
            p.Move t
            display p
            // remove "dead" particles

        particles <- List.filter (fun p -> p.Age < lifeSpan) particles
        base.SwapBuffers()


// Add a new particle in the list every 15ms.
Async.Start (async {
   while true do
     let p = Particle(rndf() * 0.1<m/s>, rndf() * 0.1<m/s>)
     particles <- p :: particles
     do! Async.Sleep 15
})

let game = new Game()
do game.Run(30.)