' Matrix multiplication benchmark
' Multiplies two NxN matrices

Const N As Long = 50

Function CreateMatrix(size As Long, seed As Long) As Variant
  ReDim mat(size * size - 1)
  Dim i As Long
  Dim val As Long
  val = seed
  For i = 0 To size * size - 1
    val = (val * 1103515245 + 12345) Mod 65536
    mat(i) = val Mod 100
  Next
  CreateMatrix = mat
End Function

Function MatMul(a As Variant, b As Variant, size As Long) As Variant
  ReDim c(size * size - 1)
  Dim i As Long, j As Long, k As Long
  Dim sum As Long
  For i = 0 To size - 1
    For j = 0 To size - 1
      sum = 0
      For k = 0 To size - 1
        sum = sum + a(i * size + k) * b(k * size + j)
      Next
      c(i * size + j) = sum
    Next
  Next
  MatMul = c
End Function

Dim t0 As Double
t0 = Timer()

Dim a As Variant, b As Variant, c As Variant
a = CreateMatrix(N, 42)
b = CreateMatrix(N, 99)
c = MatMul(a, b, N)

Dim elapsed As Double
elapsed = Timer() - t0

print("Matrix", N, "x", N, "multiply")
print("c(0,0) =", c(0))
print("c(N-1,N-1) =", c(N * N - 1))
print("Time:", FormatNumber(elapsed, 3), "sec")
