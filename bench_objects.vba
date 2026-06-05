' Object-heavy benchmark: particle simulation
' Creates many objects, updates them in a loop

Const NUM_PARTICLES As Long = 500
Const NUM_STEPS As Long = 100

Class Particle
  Public X As Double
  Public Y As Double
  Public VX As Double
  Public VY As Double
  Public Mass As Double

  Sub Init(px As Double, py As Double, pvx As Double, pvy As Double, pm As Double)
    X = px
    Y = py
    VX = pvx
    VY = pvy
    Mass = pm
  End Sub

  Sub Update(dt As Double)
    ' Apply gravity
    VY = VY - 9.81 * dt
    ' Update position
    X = X + VX * dt
    Y = Y + VY * dt
    ' Bounce off floor
    If Y < 0 Then
      Y = -Y
      VY = -VY * 0.8
    End If
    ' Wrap horizontally
    If X > 1000 Then X = X - 1000
    If X < 0 Then X = X + 1000
  End Sub

  Function KineticEnergy() As Double
    KineticEnergy = 0.5 * Mass * (VX * VX + VY * VY)
  End Function
End Class

Dim t0 As Double
t0 = Timer()

' Create particles
ReDim particles(NUM_PARTICLES - 1)
Dim i As Long
Dim seed As Long
seed = 12345
For i = 0 To NUM_PARTICLES - 1
  Set particles(i) = New Particle
  seed = (seed * 1103515245 + 12345) Mod 65536
  Dim px As Double
  px = (seed Mod 1000)
  seed = (seed * 1103515245 + 12345) Mod 65536
  Dim py As Double
  py = (seed Mod 500)
  seed = (seed * 1103515245 + 12345) Mod 65536
  Dim pvx As Double
  pvx = (seed Mod 200) - 100
  seed = (seed * 1103515245 + 12345) Mod 65536
  Dim pvy As Double
  pvy = (seed Mod 200) - 100
  particles(i).Init px, py, pvx, pvy, 1.0
Next

' Simulate
Dim st As Long
Dim dt As Double
dt = 0.016
For st = 1 To NUM_STEPS
  For i = 0 To NUM_PARTICLES - 1
    particles(i).Update dt
  Next
Next

' Compute total energy
Dim totalKE As Double
totalKE = 0
For i = 0 To NUM_PARTICLES - 1
  totalKE = totalKE + particles(i).KineticEnergy()
Next

Dim elapsed As Double
elapsed = Timer() - t0

print("Particle simulation:", NUM_PARTICLES, "particles,", NUM_STEPS, "steps")
print("Total KE:", FormatNumber(totalKE, 2))
print("First pos:", FormatNumber(particles(0).X, 2), FormatNumber(particles(0).Y, 2))
print("Time:", FormatNumber(elapsed, 3), "sec")
