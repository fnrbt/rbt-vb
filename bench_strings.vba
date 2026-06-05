' String-heavy benchmark
' Builds, manipulates, and searches strings

Const ITERATIONS As Long = 5000

Dim t0 As Double
t0 = Timer()

' 1. String building with concatenation
Dim built As String
built = ""
Dim i As Long
For i = 1 To ITERATIONS
  built = built & Chr(65 + (i Mod 26))
Next
print("Built string length:", Len(built))

' 2. Count character occurrences
Dim counts As Long
counts = 0
For i = 1 To Len(built)
  If Mid(built, i, 1) = "A" Then
    counts = counts + 1
  End If
Next
print("Count of A:", counts)

' 3. Replace operations
Dim replaced As String
replaced = Replace(built, "ABC", "XYZ")
print("After replace length:", Len(replaced))

' 4. Split and rejoin
Dim words As String
words = ""
For i = 1 To 1000
  If words <> "" Then words = words & " "
  words = words & "word" & CStr(i)
Next
Dim parts As Variant
parts = Split(words, " ")
print("Word count:", UBound(parts) + 1)
Dim rejoined As String
rejoined = Join(parts, "-")
print("Rejoined length:", Len(rejoined))

' 5. Palindrome checking on substrings
Dim palindromes As Long
palindromes = 0
For i = 1 To 500
  Dim sub1 As String
  sub1 = Mid(built, i, 5)
  If sub1 = StrReverse(sub1) Then
    palindromes = palindromes + 1
  End If
Next
print("Palindromes found:", palindromes)

Dim elapsed As Double
elapsed = Timer() - t0
print("Time:", FormatNumber(elapsed, 3), "sec")
