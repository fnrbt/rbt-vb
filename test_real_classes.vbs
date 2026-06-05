' Real-world VBScript class examples
' Sourced from herongyang.com, tutorialspoint.com, and various VBScript tutorials

' ============================================================
' 1. Node class with Property Let/Get (from herongyang.com)
' ============================================================
Class Node
  Public Title
  Private m_User
  Private m_Domain

  Public Property Let Email(sEmail)
    Dim at
    at = InStr(sEmail, "@")
    If at > 0 Then
      m_User = Mid(sEmail, 1, at - 1)
      m_Domain = Mid(sEmail, at + 1, Len(sEmail) - at)
    End If
  End Property

  Public Property Get Email()
    Email = m_User & "@" & m_Domain
  End Property

  Sub Class_Initialize()
    Title = "Default Node"
    m_User = "info"
    m_Domain = "example.com"
  End Sub
End Class

Dim oNode
Set oNode = New Node
print("Initial title:", oNode.Title)
print("Initial email:", oNode.Email)

oNode.Title = "Support Desk"
oNode.Email = "help@microsoft.com"
print("Updated title:", oNode.Title)
print("Updated email:", oNode.Email)

' ============================================================
' 2. Stack class (inspired by 4guysfromrolla.com)
' ============================================================
Class Stack
  Private m_items
  Private m_count

  Sub Class_Initialize()
    m_count = 0
    ReDim m_items(10)
  End Sub

  Sub Push(value)
    If m_count >= UBound(m_items) Then
      ReDim Preserve m_items(m_count * 2)
    End If
    m_items(m_count) = value
    m_count = m_count + 1
  End Sub

  Function Pop()
    If m_count = 0 Then
      Pop = Empty
      Exit Function
    End If
    m_count = m_count - 1
    Pop = m_items(m_count)
  End Function

  Function Peek()
    If m_count = 0 Then
      Peek = Empty
      Exit Function
    End If
    Peek = m_items(m_count - 1)
  End Function

  Property Get Count()
    Count = m_count
  End Property

  Property Get IsEmpty()
    IsEmpty = (m_count = 0)
  End Property
End Class

Dim stk
Set stk = New Stack
stk.Push("first")
stk.Push("second")
stk.Push("third")
print("Stack count:", stk.Count)
print("Peek:", stk.Peek())
print("Pop:", stk.Pop())
print("Pop:", stk.Pop())
print("Count after pops:", stk.Count)
print("IsEmpty:", stk.IsEmpty)
print("Pop last:", stk.Pop())
print("IsEmpty:", stk.IsEmpty)

' ============================================================
' 3. Bank Account class
' ============================================================
Class BankAccount
  Private m_owner
  Private m_balance
  Private m_transactions

  Sub Class_Initialize()
    m_owner = ""
    m_balance = 0
    m_transactions = 0
  End Sub

  Property Let Owner(name)
    m_owner = name
  End Property

  Property Get Owner()
    Owner = m_owner
  End Property

  Property Get Balance()
    Balance = m_balance
  End Property

  Property Get TransactionCount()
    TransactionCount = m_transactions
  End Property

  Function Deposit(amount)
    If amount > 0 Then
      m_balance = m_balance + amount
      m_transactions = m_transactions + 1
      Deposit = True
    Else
      Deposit = False
    End If
  End Function

  Function Withdraw(amount)
    If amount > 0 And amount <= m_balance Then
      m_balance = m_balance - amount
      m_transactions = m_transactions + 1
      Withdraw = True
    Else
      Withdraw = False
    End If
  End Function

  Function ToString()
    ToString = m_owner & ": $" & FormatNumber(m_balance, 2) & " (" & m_transactions & " txns)"
  End Function
End Class

Dim acct
Set acct = New BankAccount
acct.Owner = "Alice"
acct.Deposit(1000)
acct.Deposit(500)
acct.Withdraw(200)
print(acct.ToString())
print("Withdraw $2000:", acct.Withdraw(2000))
print("Final:", acct.ToString())

' ============================================================
' 4. Linked list using classes
' ============================================================
Class ListNode
  Public Value
  Public NextNode

  Sub Class_Initialize()
    Value = Empty
    Set NextNode = Nothing
  End Sub
End Class

Class LinkedList
  Private m_head
  Private m_size

  Sub Class_Initialize()
    Set m_head = Nothing
    m_size = 0
  End Sub

  Sub AddFirst(val)
    Dim node
    Set node = New ListNode
    node.Value = val
    Set node.NextNode = m_head
    Set m_head = node
    m_size = m_size + 1
  End Sub

  Sub AddLast(val)
    Dim node
    Set node = New ListNode
    node.Value = val
    If m_head Is Nothing Then
      Set m_head = node
    Else
      Dim current
      Set current = m_head
      Do While Not (current.NextNode Is Nothing)
        Set current = current.NextNode
      Loop
      Set current.NextNode = node
    End If
    m_size = m_size + 1
  End Sub

  Function RemoveFirst()
    If m_head Is Nothing Then
      RemoveFirst = Empty
      Exit Function
    End If
    RemoveFirst = m_head.Value
    Set m_head = m_head.NextNode
    m_size = m_size - 1
  End Function

  Function ToArray()
    ReDim result(m_size - 1)
    Dim current, idx
    Set current = m_head
    idx = 0
    Do While Not (current Is Nothing)
      result(idx) = current.Value
      Set current = current.NextNode
      idx = idx + 1
    Loop
    ToArray = result
  End Function

  Property Get Size()
    Size = m_size
  End Property

  Function ToString()
    Dim s, current
    s = ""
    Set current = m_head
    Do While Not (current Is Nothing)
      If s <> "" Then s = s & " -> "
      s = s & CStr(current.Value)
      Set current = current.NextNode
    Loop
    ToString = s
  End Function
End Class

Dim list
Set list = New LinkedList
list.AddLast(10)
list.AddLast(20)
list.AddLast(30)
list.AddFirst(5)
print("List:", list.ToString())
print("Size:", list.Size)
print("Remove first:", list.RemoveFirst())
print("List after remove:", list.ToString())

Dim listArr
listArr = list.ToArray()
print("As array:", listArr(0), listArr(1), listArr(2))
