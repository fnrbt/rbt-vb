/// Loop-heavy VBA programs for timing the engine on .NET and under Fable.
/// Each is a project: modules, an entry Sub, and the Debug.Print output it
/// must produce (so both runtimes are checked to compute the same thing).
module Rbt.Vb.Bench.Programs

type BenchProgram = {
    Name: string
    Modules: (string * bool * string) list   // name, is class module, source
    Entry: string
}

let private standard name source = name, false, source
let private classModule name source = name, true, source

/// Rosetta Code's VBA Abelian sandpile (tests/macros/showcase/sandpile.bas in
/// rbt-office) without the sheet: implicit Variants, an Integer 2-D array,
/// one topple per pass (GoTo), While/Wend.
let sandpile = {
    Name = "sandpile"
    Entry = "Main"
    Modules = [ standard "Module1" """
Sub Main()
Dim PileWidth As Integer
Dim PileHeight As Integer
Dim FieldArray() As Integer
PileWidth = 25
PileHeight = 25
ReDim FieldArray(PileWidth - 1, PileHeight - 1)
SandDropAmount = 400
SandDropColumn = Round((PileWidth / 2) + 0.001, 0)
SandDropRow = Round((PileHeight / 2) + 0.001, 0)
FieldArray(SandDropRow - 1, SandDropColumn - 1) = SandDropAmount
Continue = False
For i = 1 To PileWidth
For j = 1 To PileHeight
If FieldArray(j - 1, i - 1) > 3 Then Continue = True
Next j
Next i
While Continue
For i = 1 To PileWidth
For j = 1 To PileHeight
    If FieldArray(j - 1, i - 1) > 3 Then
    FieldArray(j - 1, i - 1) = FieldArray(j - 1, i - 1) - 4
    If j >= 2 Then FieldArray(j - 2, i - 1) = FieldArray(j - 2, i - 1) + 1
    If i < PileWidth Then FieldArray(j - 1, i) = FieldArray(j - 1, i) + 1
    If j < PileHeight Then FieldArray(j, i - 1) = FieldArray(j, i - 1) + 1
    If i >= 2 Then FieldArray(j - 1, i - 2) = FieldArray(j - 1, i - 2) + 1
    GoTo Nextone
    End If
Next j
Next i
Nextone:
Continue = False
For i = 1 To PileWidth
For j = 1 To PileHeight
If FieldArray(j - 1, i - 1) > 3 Then Continue = True
Next j
Next i
Wend
total = 0
For i = 0 To PileWidth - 1
For j = 0 To PileHeight - 1
total = total + FieldArray(j, i) * (i + 1) + j
Next j
Next i
Debug.Print total
End Sub
""" ]
}

/// Sieve of Eratosthenes on a declared Long array, Long counters.
let sieve = {
    Name = "sieve"
    Entry = "Main"
    Modules = [ standard "Module1" """
Option Explicit
Sub Main()
    Const LIMIT As Long = 300000
    Dim flags() As Boolean
    ReDim flags(LIMIT)
    Dim i As Long, j As Long, count As Long, last As Long
    For i = 2 To LIMIT
        flags(i) = True
    Next
    i = 2
    Do While i * i <= LIMIT
        If flags(i) Then
            For j = i * i To LIMIT Step i
                flags(j) = False
            Next
        End If
        i = i + 1
    Loop
    For i = 2 To LIMIT
        If flags(i) Then
            count = count + 1
            last = i
        End If
    Next
    Debug.Print count; last
End Sub
""" ]
}

/// Matrix product on Double and Long 2-D arrays, and a Variant array.
let matrix = {
    Name = "matrix"
    Entry = "Main"
    Modules = [ standard "Module1" """
Option Explicit
Sub Main()
    Const N As Long = 60
    Dim a() As Double, b() As Double, c() As Double
    ReDim a(1 To N, 1 To N): ReDim b(1 To N, 1 To N): ReDim c(1 To N, 1 To N)
    Dim m() As Long
    ReDim m(N, N)
    Dim v As Variant
    v = Array(1, 2, 3, 4, 5, 6, 7, 8)
    Dim i As Long, j As Long, k As Long, seed As Long
    seed = 42
    For i = 1 To N
        For j = 1 To N
            seed = (seed * 25173 + 13849) Mod 65536
            a(i, j) = (seed Mod 100) / 10
            b(j, i) = (seed Mod 37) - 18
            m(i - 1, j - 1) = seed Mod 1000
        Next
    Next
    Dim s As Double, t As Long
    For i = 1 To N
        For j = 1 To N
            s = 0
            For k = 1 To N
                s = s + a(i, k) * b(k, j)
                t = t + m(k - 1, j - 1) Mod 7 + v(k Mod 8)
            Next
            c(i, j) = s
        Next
    Next
    Debug.Print Round(c(1, 1), 4); Round(c(N, N), 4); t
End Sub
""" ]
}

/// Recursion: argument passing and returns (Fib, Ackermann, Hanoi).
let recursion = {
    Name = "recursion"
    Entry = "Main"
    Modules = [ standard "Module1" """
Option Explicit
Dim moves As Long
Function Fib(ByVal n As Long) As Long
    If n < 2 Then Fib = n Else Fib = Fib(n - 1) + Fib(n - 2)
End Function
Function Ack(ByVal m As Long, ByVal n As Long) As Long
    If m = 0 Then
        Ack = n + 1
    ElseIf n = 0 Then
        Ack = Ack(m - 1, 1)
    Else
        Ack = Ack(m - 1, Ack(m, n - 1))
    End If
End Function
Sub Hanoi(n As Integer, a As String, b As String, c As String)
    If n = 0 Then Exit Sub
    Hanoi n - 1, a, c, b
    moves = moves + 1
    Hanoi n - 1, c, b, a
End Sub
Sub Main()
    Debug.Print Fib(24); Ack(2, 300); 
    Hanoi 16, "A", "B", "C"
    Debug.Print moves
End Sub
""" ]
}

/// Variant (undeclared) integer arithmetic: + - * \ Mod with widening.
let variantMath = {
    Name = "variant-math"
    Entry = "Main"
    Modules = [ standard "Module1" """
Sub Main()
    total = 0
    acc = 1
    For i = 1 To 200000
        x = i Mod 1000
        y = (x * 3 + 7) \ 2
        total = total + y - x
        acc = (acc * 31 + i) Mod 1000003
        If total > 100000000 Then total = total - 100000000
    Next
    big = 2147483600
    For i = 1 To 100
        big = big + 1
    Next
    Debug.Print total; acc; big
End Sub
""" ]
}

/// Strings: concatenation, Mid, Len, InStr, Asc/Chr.
let strings = {
    Name = "strings"
    Entry = "Main"
    Modules = [ standard "Module1" """
Option Explicit
Sub Main()
    Dim s As String, i As Long, n As Long, h As Long
    For i = 1 To 20000
        s = s & Chr(65 + i Mod 26)
    Next
    For i = 1 To Len(s) Step 3
        h = (h * 7 + Asc(Mid(s, i, 1))) Mod 100003
    Next
    For i = 1 To 2000
        n = n + InStr(i, s, "XYZ")
    Next
    Debug.Print Len(s); h; n
End Sub
""" ]
}

/// A class with fields, property procedures and methods, in an object array.
let objects = {
    Name = "objects"
    Entry = "Main"
    Modules = [
        classModule "Particle" """
Option Explicit
Public X As Double
Public Y As Double
Private mVX As Double
Private mVY As Double
Public Sub Init(px As Double, py As Double, vx As Double, vy As Double)
    X = px: Y = py: mVX = vx: mVY = vy
End Sub
Public Sub Update(dt As Double)
    mVY = mVY - 9.81 * dt
    X = X + mVX * dt
    Y = Y + mVY * dt
    If Y < 0 Then Y = -Y: mVY = -mVY * 0.8
    If X > 1000 Then X = X - 1000
    If X < 0 Then X = X + 1000
End Sub
Public Property Get Speed() As Double
    Speed = Sqr(mVX * mVX + mVY * mVY)
End Property
"""
        standard "Module1" """
Option Explicit
Sub Main()
    Const N As Long = 300
    Dim ps(N - 1) As Particle
    Dim i As Long, st As Long, seed As Long
    seed = 12345
    For i = 0 To N - 1
        Set ps(i) = New Particle
        seed = (seed * 25173 + 13849) Mod 65536
        ps(i).Init seed Mod 1000, seed Mod 500, (seed Mod 200) - 100, (seed Mod 77) - 30
    Next
    For st = 1 To 60
        For i = 0 To N - 1
            ps(i).Update 0.016
        Next
    Next
    Dim total As Double
    For i = 0 To N - 1
        total = total + ps(i).Speed
    Next
    Debug.Print Round(total, 3); Round(ps(0).X, 3)
End Sub
""" ]
}

let all = [ sandpile; sieve; matrix; recursion; variantMath; strings; objects ]
