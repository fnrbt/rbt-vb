Sub TestErrors()
    On Error Resume Next
    On Error GoTo ErrorHandler
    On Error GoTo 0
    print("error handling test")
End Sub
