Option Explicit

' Declare an API function
Private Declare Function GetTickCount Lib "kernel32" () As Long
Public Declare Sub Sleep Lib "kernel32" Alias "Sleep" (ByVal ms As Long)

Class EventSource
    Public Event DataReady(ByVal data As String)
    Private WithEvents m_timer As Timer

    Public Enum Priority
        Low = 0
        Medium = 1
        High = 2
    End Enum

    Public Sub DoWork()
        print("working")
    End Sub
End Class
