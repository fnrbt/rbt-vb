' Recursive algorithm benchmarks

Dim t0 As Double

' 1. Fibonacci (exponential recursion)
Function Fib(n As Long) As Long
  If n < 2 Then
    Fib = n
  Else
    Fib = Fib(n - 1) + Fib(n - 2)
  End If
End Function

t0 = Timer()
Dim fibResult As Long
fibResult = Fib(28)
print("Fib(28) =", fibResult, "Time:", FormatNumber(Timer() - t0, 3), "sec")

' 2. Ackermann function (deeply recursive)
Function Ack(m As Long, n As Long) As Long
  If m = 0 Then
    Ack = n + 1
  ElseIf n = 0 Then
    Ack = Ack(m - 1, 1)
  Else
    Ack = Ack(m - 1, Ack(m, n - 1))
  End If
End Function

t0 = Timer()
Dim ackResult As Long
ackResult = Ack(3, 7)
print("Ack(3,7) =", ackResult, "Time:", FormatNumber(Timer() - t0, 3), "sec")

' 3. Tower of Hanoi (count moves)
Dim moveCount As Long

Sub Hanoi(n As Long, src As String, dst As String, aux As String)
  If n = 0 Then Exit Sub
  Hanoi n - 1, src, aux, dst
  moveCount = moveCount + 1
  Hanoi n - 1, aux, dst, src
End Sub

moveCount = 0
t0 = Timer()
Hanoi 20, "A", "C", "B"
print("Hanoi(20) moves:", moveCount, "Time:", FormatNumber(Timer() - t0, 3), "sec")

' 4. QuickSort on large array
Sub QuickSort(arr As Variant, lo As Long, hi As Long)
  If lo >= hi Then Exit Sub
  Dim pivot As Long, i As Long, j As Long, temp As Long
  pivot = arr(hi)
  i = lo
  For j = lo To hi - 1
    If arr(j) <= pivot Then
      temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
      i = i + 1
    End If
  Next
  temp = arr(i)
  arr(i) = arr(hi)
  arr(hi) = temp
  QuickSort arr, lo, i - 1
  QuickSort arr, i + 1, hi
End Sub

Const ARR_SIZE As Long = 5000
ReDim data(ARR_SIZE - 1)
Dim seed As Long
seed = 42
Dim k As Long
For k = 0 To ARR_SIZE - 1
  seed = (seed * 1103515245 + 12345) Mod 65536
  data(k) = seed
Next

t0 = Timer()
QuickSort data, 0, ARR_SIZE - 1
Dim sorted As Boolean
sorted = True
For k = 0 To ARR_SIZE - 2
  If data(k) > data(k + 1) Then
    sorted = False
    Exit For
  End If
Next
print("QuickSort", ARR_SIZE, "elements, sorted:", sorted, "Time:", FormatNumber(Timer() - t0, 3), "sec")
