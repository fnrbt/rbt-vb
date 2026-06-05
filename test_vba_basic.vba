Option Explicit

Dim x As Integer
Dim name As String
Dim arr(10) As String

Public total As Long
Private helper As Double

Const PI = 3.14159

Function Add(ByVal a As Integer, ByVal b As Integer) As Integer
    Add = a + b
End Function

Sub PrintMessage(ByVal msg As String)
    print(msg)
End Sub

Property Get Value() As Integer
    Value = 42
End Property

Public Enum Colors
    Red = 1
    Green = 2
    Blue = 3
End Enum

Private Type Point
    x As Integer
    y As Integer
End Type

Class MyClass
    Private m_value As Integer

    Public Property Get Value() As Integer
        Value = m_value
    End Property

    Public Property Let Value(ByVal v As Integer)
        m_value = v
    End Property

    Public Sub DoSomething()
        print("doing something")
    End Sub
End Class
