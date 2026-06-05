' Sieve of Eratosthenes benchmark
' Find all primes up to LIMIT

Const LIMIT As Long = 100000

Dim t0 As Double
t0 = Timer()

ReDim sieve(LIMIT)
Dim i As Long, j As Long
For i = 0 To LIMIT
  sieve(i) = 1
Next
sieve(0) = 0
sieve(1) = 0

i = 2
Do While i * i <= LIMIT
  If sieve(i) = 1 Then
    j = i * i
    Do While j <= LIMIT
      sieve(j) = 0
      j = j + i
    Loop
  End If
  i = i + 1
Loop

Dim count As Long
count = 0
Dim lastPrime As Long
For i = 2 To LIMIT
  If sieve(i) = 1 Then
    count = count + 1
    lastPrime = i
  End If
Next

Dim elapsed As Double
elapsed = Timer() - t0

print("Sieve of Eratosthenes up to", LIMIT)
print("Primes found:", count)
print("Largest prime:", lastPrime)
print("Time:", FormatNumber(elapsed, 3), "sec")
