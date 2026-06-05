Function Greet(Optional ByVal name As String = "World") As String
    Greet = "Hello, " & name
End Function

Sub PrintAll(ParamArray items() As String)
    print("items")
End Sub

Private Function Calculate(ByVal x As Double, ByVal y As Double, Optional ByVal op As String = "add") As Double
    Calculate = x + y
End Function
