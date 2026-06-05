' Real-world algorithmic VBScript programs

' ============================================================
' 1. Bubble Sort (from helloacm.com)
' ============================================================
Dim arr
arr = Array(4, 6, 2, 7, 3, 5, 1, 8, 10, 22, 33, 15, 11, 8)

Dim i, j, TempValue
For i = 0 To UBound(arr)
  For j = 0 To UBound(arr) - 1
    If arr(j) > arr(j + 1) Then
      TempValue = arr(j + 1)
      arr(j + 1) = arr(j)
      arr(j) = TempValue
    End If
  Next
Next

Dim s
s = ""
For i = 0 To UBound(arr)
  If s <> "" Then
    s = s & ","
  End If
  s = s & arr(i)
Next
print("Sorted:", s)

' ============================================================
' 2. Factorial
' ============================================================
Function Factorial(n)
  Dim f
  f = 1
  Dim k
  For k = n To 1 Step -1
    f = f * k
  Next
  Factorial = f
End Function

print("5! =", Factorial(5))
print("10! =", Factorial(10))

' ============================================================
' 3. Recursive Fibonacci
' ============================================================
Function Fibonacci(N)
  If N < 2 Then
    Fibonacci = N
  Else
    Fibonacci = Fibonacci(N - 1) + Fibonacci(N - 2)
  End If
End Function

Dim fib_result
fib_result = ""
For i = 0 To 12
  If fib_result <> "" Then
    fib_result = fib_result & ","
  End If
  fib_result = fib_result & Fibonacci(i)
Next
print("Fibonacci:", fib_result)

' ============================================================
' 4. Prime number sieve
' ============================================================
Function IsPrime(num)
  If num < 2 Then
    IsPrime = False
    Exit Function
  End If
  If num = 2 Then
    IsPrime = True
    Exit Function
  End If
  If num Mod 2 = 0 Then
    IsPrime = False
    Exit Function
  End If
  Dim d
  d = 3
  Do While d * d <= num
    If num Mod d = 0 Then
      IsPrime = False
      Exit Function
    End If
    d = d + 2
  Loop
  IsPrime = True
End Function

Dim primes
primes = ""
For i = 2 To 50
  If IsPrime(i) Then
    If primes <> "" Then
      primes = primes & ","
    End If
    primes = primes & i
  End If
Next
print("Primes to 50:", primes)

' ============================================================
' 5. Palindrome check
' ============================================================
Function IsPalindrome(str)
  Dim reversed
  reversed = StrReverse(str)
  IsPalindrome = (LCase(str) = LCase(reversed))
End Function

print("racecar:", IsPalindrome("racecar"))
print("hello:", IsPalindrome("hello"))
print("Madam:", IsPalindrome("Madam"))

' ============================================================
' 6. Extract numbers from string
' ============================================================
Function ExtractNumbers(str)
  Dim result, ch
  result = ""
  Dim p
  For p = 1 To Len(str)
    ch = Mid(str, p, 1)
    If IsNumeric(ch) Then
      result = result & ch
    End If
  Next
  ExtractNumbers = result
End Function

print("Numbers in 'abc123def456':", ExtractNumbers("abc123def456"))
