' Real-world VBA programs with VBA-only features
' Tests typed declarations, GoTo error handling, labels, etc.

' ============================================================
' 1. QuickSort with typed parameters
' ============================================================
Sub QuickSort(arr, ByVal lo As Long, ByVal hi As Long)
  If lo >= hi Then Exit Sub
  Dim pivot, i As Long, j As Long, temp
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

Dim data
data = Array(38, 27, 43, 3, 9, 82, 10)
QuickSort data, 0, UBound(data)
Dim result
result = ""
Dim k
For k = 0 To UBound(data)
  If result <> "" Then result = result & ","
  result = result & data(k)
Next
print("QuickSorted:", result)

' ============================================================
' 2. Error handling with GoTo
' ============================================================
Sub SafeDivide(a, b)
  On Error GoTo DivError
  Dim result
  result = a / b
  print("Result:", a, "/", b, "=", result)
  Exit Sub
DivError:
  print("Error dividing", a, "by", b)
End Sub

SafeDivide 10, 3
SafeDivide 10, 0

' ============================================================
' 3. GoSub pattern for reusable code blocks
' ============================================================
Sub ProcessItems()
  Dim items
  items = Array("apple", "BANANA", "Cherry", "DATE")
  Dim i
  For i = 0 To UBound(items)
    Dim current
    current = items(i)
    GoSub NormalizeAndPrint
  Next
  Exit Sub

NormalizeAndPrint:
  print("Item:", LCase(Trim(current)))
  Return
End Sub

ProcessItems

' ============================================================
' 4. Typed constants and enums
' ============================================================
Public Const MAX_SIZE As Long = 100
Public Const PI As Double = 3.14159265358979
Private Const APP_NAME As String = "TestApp"

print("Max:", MAX_SIZE, "Pi:", Round(PI, 4), "App:", APP_NAME)

' ============================================================
' 5. Calculator with class and method dispatch
' ============================================================
Class Calculator
  Private m_memory As Double
  Private m_last As Double

  Sub Class_Initialize()
    m_memory = 0
    m_last = 0
  End Sub

  Function Add(a As Double, b As Double) As Double
    m_last = a + b
    Add = m_last
  End Function

  Function Subtract(a As Double, b As Double) As Double
    m_last = a - b
    Subtract = m_last
  End Function

  Function MultiplyVal(a As Double, b As Double) As Double
    m_last = a * b
    MultiplyVal = m_last
  End Function

  Sub MemoryStore()
    m_memory = m_last
  End Sub

  Sub MemoryRecall()
    m_last = m_memory
  End Sub

  Property Get LastResult() As Double
    LastResult = m_last
  End Property

  Property Get Memory() As Double
    Memory = m_memory
  End Property
End Class

Dim calc
Set calc = New Calculator
print("3 + 4 =", calc.Add(3, 4))
print("10 - 3 =", calc.Subtract(10, 3))
print("6 * 7 =", calc.MultiplyVal(6, 7))
calc.MemoryStore()
print("Memory:", calc.Memory)
print("Last:", calc.LastResult)
