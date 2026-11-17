module Rbt.Vb.Tests.ReleaseCases

// 0.3.0 regressions: silent wrong results, error numbers, declared types, the
// host bridge and the project/session embedding API. Shared by .NET and Fable.

open Rbt.Vb
open Rbt.Vb.Api
open Rbt.Vb.Values

let private fail label expected actual =
    failwithf "%s: expected %A, received %A" label expected actual

let private run options source =
    match runSource options source with
    | Ok result -> result
    | Error error -> failwithf "Execution failed: %A\n%s" error source

let private globalOf name (result: RunResult) =
    match tryGetGlobal name result.State with
    | Some value -> value
    | None -> failwithf "Missing global %s" name

let private vba source name expected =
    let actual = globalOf name (run RunOptions.vba source)
    if actual <> expected then fail (name + " in\n" + source) expected actual

let private vbaWith options source name expected =
    let actual = globalOf name (run options source)
    if actual <> expected then fail (name + " in\n" + source) expected actual

/// `result = <expression>` in VBA.
let private expr expression expected = vba ("Dim result\nresult = " + expression) "result" expected

/// The Err.Number an unhandled-by-default statement raises, caught by Resume Next.
let private errNumber setup statement expected =
    vba (sprintf "Dim result\n%s\nOn Error Resume Next\n%s\nresult = Err.Number" setup statement) "result" (VInteger expected)

let private runtimeFailure options source =
    match runSource options source with
    | Error (RunError.RuntimeError message) -> message
    | other -> failwithf "Expected runtime error, received %A\n%s" other source

let private contains (text: string) (part: string) =
    if not (text.Contains part) then failwithf "Expected %A to contain %A" text part

// ── A fake spreadsheet host ──

type private Sheet() =
    member val Cells = System.Collections.Generic.Dictionary<string, Value>()
    member val Log = ResizeArray<string>()

let rec private cellObject (sheet: Sheet) (address: string) : Value =
    let read () = match sheet.Cells.TryGetValue address with | true, v -> v | _ -> VEmpty
    VHostObject
        { new IHostObject with
            member _.TypeName = "Range"
            member _.GetMember name =
                match name with
                | "" | "Value" -> Some (read ())
                | "Address" -> Some (VString address)
                | _ -> None
            member _.SetMember name value =
                match name with
                | "" | "Value" -> sheet.Cells.[address] <- value; true
                | _ -> false
            member _.CallMethod name args =
                match name with
                | "Copy" ->
                    let target =
                        match args with
                        | [| VHostObject h |] -> (match h.GetMember "Address" with Some (VString a) -> a | _ -> "?")
                        | _ -> sprintf "%A" args
                    sheet.Log.Add("Copy " + address + "->" + target)
                    Some VEmpty
                | "Address" ->
                    let describe v = match v with VUndefined -> "missing" | other -> ValueSemantics.toString other
                    sheet.Log.Add("Address(" + (args |> Array.map describe |> String.concat ",") + ")")
                    Some (VString address)
                | "Fail" -> raiseVbErrorWith 1004 "Range failure"
                | _ -> None
            member this.CallNamed name args names =
                match name with
                | "Find" ->
                    let parts = Array.map2 (fun (n: string) v -> n + "=" + ValueSemantics.toString v) names args
                    sheet.Log.Add("Find " + String.concat ";" parts)
                    Some VEmpty
                | _ -> this.CallMethod name args
            member _.Enumerate () = None }

let private rangeFunction (sheet: Sheet) =
    VHostObject
        { new IHostObject with
            member _.TypeName = "RangeFunction"
            member _.GetMember _ = None
            member _.SetMember _ _ = false
            member _.CallMethod name args =
                match name, args with
                | "", [| VString address |] -> Some (cellObject sheet address)
                | "", [| row; col |] ->
                    Some (cellObject sheet (sprintf "R%dC%d" (ValueSemantics.toInteger row) (ValueSemantics.toInteger col)))
                | _ -> None
            member this.CallNamed name args _ = this.CallMethod name args
            member _.Enumerate () = None }

let private worksheet (sheet: Sheet) =
    let range = rangeFunction sheet
    VHostObject
        { new IHostObject with
            member _.TypeName = "Worksheet"
            member _.GetMember name = if name = "Name" then Some (VString "Sheet1") else None
            member _.SetMember _ _ = false
            member _.CallMethod name args =
                match name, range with
                | ("Range" | "Cells"), VHostObject r -> r.CallMethod "" args
                | _ -> None
            member this.CallNamed name args _ = this.CallMethod name args
            member _.Enumerate () = Some (seq { VInteger 1; VInteger 2; VInteger 3 }) }

let private sheetOptions (sheet: Sheet) =
    RunOptions.vba
    |> RunOptions.withHostGlobals (Map.ofList [
        "Range", rangeFunction sheet
        "Cells", rangeFunction sheet
        "ActiveSheet", worksheet sheet
        "xlUp", VInteger -4162 ])

let private sheetRun (sheet: Sheet) source = run (sheetOptions sheet) source |> ignore

let private expectCell (sheet: Sheet) address expected =
    match sheet.Cells.TryGetValue address with
    | true, v when v = expected -> ()
    | true, v -> fail ("cell " + address) expected v
    | _ -> fail ("cell " + address) expected "unset"

let private expectLog (sheet: Sheet) (expected: string list) =
    let actual = List.ofSeq sheet.Log
    if actual <> expected then fail "host log" expected actual

// ── Projects ──

let private module1 =
    "Attribute VB_Name = \"Module1\"\n" +
    "Option Explicit\n" +
    "Public Counter As Long\n" +
    "Private hidden As Long\n" +
    "\n" +
    "Public Sub Inc()\n" +
    "    Counter = Counter + Helper()\n" +
    "End Sub\n" +
    "\n" +
    "Private Function Helper() As Long\n" +
    "    hidden = hidden + 1\n" +
    "    Helper = 1\n" +
    "End Function\n" +
    "\n" +
    "Sub WithArgs(a As Long, Optional b As Long = 5)\n" +
    "    Counter = a + b\n" +
    "End Sub\n" +
    "\n" +
    "Sub Boom()\n" +
    "    Dim x As Long\n" +
    "    x = 1 / 0\n" +
    "End Sub\n" +
    "\n" +
    "Sub Forever()\n" +
    "    Do\n" +
    "        Counter = Counter + 1\n" +
    "    Loop\n" +
    "End Sub\n" +
    "\n" +
    "Sub Talk()\n" +
    "    Debug.Print \"a\"; 1; \"b\", 2\n" +
    "    Counter = MsgBox(\"Continue?\", vbYesNo, \"Title\")\n" +
    "    Debug.Print InputBox(\"Name?\", \"T\", \"default\")\n" +
    "    Dim o\n" +
    "    Set o = CreateObject(\"Fake.Thing\")\n" +
    "    Debug.Print TypeName(o)\n" +
    "End Sub\n"

let private module2 =
    "Attribute VB_Name = \"Module2\"\n" +
    "Private Function Helper() As Long\n" +
    "    Helper = 100\n" +
    "End Function\n" +
    "Public Function Twice(x)\n" +
    "    Twice = x * 2 + Helper() - 100\n" +
    "End Function\n" +
    "Sub UseOther()\n" +
    "    Module1.Counter = Module1.Counter + Twice(1)\n" +
    "End Sub\n"

let private counterClass =
    "VERSION 1.0 CLASS\n" +
    "BEGIN\n" +
    "  MultiUse = -1  'True\n" +
    "END\n" +
    "Attribute VB_Name = \"Tally\"\n" +
    "Private total As Long\n" +
    "Public Sub Add(n As Long)\n" +
    "    total = total + n\n" +
    "End Sub\n" +
    "Public Property Get Value() As Long\n" +
    "    Value = total\n" +
    "End Property\n"

let private module3 =
    "Sub UseClass()\n" +
    "    Dim t As Tally\n" +
    "    Set t = New Tally\n" +
    "    t.Add 4\n" +
    "    t.Add 5\n" +
    "    Counter = t.Value\n" +
    "End Sub\n"

let private projectWith (hooks: RuntimeHooks) =
    let options = ProjectOptions.vba |> ProjectOptions.withHooks hooks
    match Project.compile options [
        ModuleSource.standard "Module1" module1
        ModuleSource.standard "Module2" module2
        ModuleSource.classModule "Tally" counterClass
        ModuleSource.standard "Module3" module3 ] with
    | Ok project -> project
    | Error errors -> failwithf "Project failed: %A" errors

let private session (project: Project) =
    match Session.Create project with
    | Ok s -> s
    | Error failure -> failwithf "Session failed: %A" failure

let private finished label status expected =
    match status with
    | Finished value when value = expected -> ()
    | other -> fail label (Finished expected) other

let private cases : (string * (unit -> unit)) list = [
    // ── 1. Silent wrong results ──
    "Enum members and qualified access", (fun () ->
        let source = "Enum Color\n  Red\n  Green = 5\n  Blue\nEnd Enum\nDim a, b, c\na = Red\nb = Blue\nc = Color.Green + Color.Blue"
        vba source "a" (VInteger 0)
        vba source "b" (VInteger 6)
        vba source "c" (VInteger 11))
    "Enum used inside a procedure", (fun () ->
        vba "Enum E\n  One = 1\n  Two\nEnd Enum\nDim result\nFunction F()\nF = E.Two * 10\nEnd Function\nresult = F()" "result" (VInteger 20))
    "Type fields store and copy by value", (fun () ->
        let source = "Type Pt\n  X As Long\n  Y As Long\nEnd Type\nDim p As Pt, q As Pt, result, copied\np.X = 3\np.Y = 4\nresult = p.X + p.Y\nq = p\nq.X = 10\ncopied = p.X"
        vba source "result" (VInteger 7)
        vba source "copied" (VInteger 3))
    "Type arrays and nested types", (fun () ->
        vba "Type Pt\n  X As Long\nEnd Type\nType Seg\n  A As Pt\n  B As Pt\n  Name As String\nEnd Type\nDim s As Seg, pts(2) As Pt, result\ns.B.X = 9\npts(1).X = 5\nresult = s.B.X + pts(1).X + pts(2).X & s.Name & \"!\"" "result" (VString "14!"))
    "Type locals in procedures", (fun () ->
        vba "Type Pt\n  X As Long\nEnd Type\nDim result\nFunction F()\nDim p As Pt\np.X = 41\nF = p.X + 1\nEnd Function\nresult = F()" "result" (VInteger 42))
    "Optional defaults and IsMissing", (fun () ->
        let source = "Dim a, b, c, d, e\nFunction F(x, Optional y = 10)\nF = x + y\nEnd Function\nFunction G(Optional v)\nG = IsMissing(v)\nEnd Function\nFunction H(Optional n As Long)\nH = n\nEnd Function\na = F(1)\nb = F(1, 2)\nc = G()\nd = G(1)\ne = H()"
        vba source "a" (VInteger 11)
        vba source "b" (VInteger 3)
        vba source "c" (VBoolean true)
        vba source "d" (VBoolean false)
        vba source "e" (VInteger 0))
    "Omitted and named arguments to procedures", (fun () ->
        let source = "Dim a, b, c\nFunction F(x, Optional y = 10, Optional z = 100)\nF = x + y + z\nEnd Function\na = F(1, , 3)\nb = F(z:=1, x:=2)\nCall Sub1(b:=2, a:=1)\nSub Sub1(a, b)\nc = a * 10 + b\nEnd Sub"
        vba source "a" (VInteger 14)
        vba source "b" (VInteger 13)
        vba source "c" (VInteger 12))
    "ParamArray collects the remaining arguments", (fun () ->
        vba "Dim result\nFunction Total(ParamArray xs())\nDim v, t\nFor Each v In xs\nt = t + v\nNext\nTotal = t\nEnd Function\nresult = Total(1, 2, 3) + Total()" "result" (VInteger 6))
    "Hex, octal, exponent and suffix literals", (fun () ->
        for expression, expected in [
            "&HFF", VInteger 255; "&H1f", VInteger 31; "&O17", VInteger 15; "&HFFFF", VInteger -1
            "&HFFFF&", VInteger 65535; "&H8000&", VInteger 32768; "&HFFFFFFFF", VInteger -1
            "1E3", VDouble 1000.0; "2.5E-1", VDouble 0.25; "5&", VInteger 5; "5#", VDouble 5.0; "5!", VDouble 5.0
            "5@", VDouble 5.0; "TypeName(5#)", VString "Double"; "&HFF + 1", VInteger 256; "Left$(\"abc\", 2)", VString "ab" ] do
            expr expression expected)
    "Nested With blocks keep their own targets", (fun () ->
        let source = "Class C\nPublic A\nPublic B\nEnd Class\nDim o1, o2, r1, r2\nSet o1 = New C\nSet o2 = New C\nWith o1\n  .A = 1\n  With o2\n    .A = 2\n  End With\n  .B = 3\nEnd With\nr1 = o1.A + o1.B\nr2 = o2.A"
        vba source "r1" (VInteger 4)
        vba source "r2" (VInteger 2))
    "Indexing a call result", (fun () ->
        expr "Split(\"a,b,c\", \",\")(1)" (VString "b")
        vba "Dim result\nFunction Arr()\nArr = Array(10, 20)\nEnd Function\nresult = Arr()(1)" "result" (VInteger 20))
    "vb constants", (fun () ->
        for expression, expected in [
            "vbCrLf = Chr(13) & Chr(10)", VBoolean true; "Len(vbTab & vbCr & vbLf & vbNewLine)", VInteger 5
            "vbNullString = \"\"", VBoolean true; "Asc(vbNullChar)", VInteger 0; "vbTrue", VInteger -1; "vbFalse", VInteger 0
            "vbTextCompare", VInteger 1; "vbBinaryCompare", VInteger 0; "vbObjectError", VInteger -2147221504
            "vbString", VInteger 8; "vbArray", VInteger 8192; "vbYesNoCancel", VInteger 3; "vbOKOnly", VInteger 0
            "vbNo", VInteger 7; "vbInformation + vbExclamation + vbCritical + vbQuestion", VInteger 160
            "VarType(\"x\") = vbString", VBoolean true ] do
            expr expression expected)
    "Arguments evaluate left to right", (fun () ->
        vba "Dim log, result\nFunction T(x)\nlog = log & x\nT = x\nEnd Function\nFunction Pair(a, b)\nPair = a & b\nEnd Function\nresult = Pair(T(1), T(2)) & Mid(\"abcdef\", T(3), T(2))" "log" (VString "1232"))
    "Recursive call result passed ByRef", (fun () ->
        vba "Dim result\nFunction Ack(m, n)\nIf m = 0 Then\nAck = n + 1\nElseIf n = 0 Then\nAck = Ack(m - 1, 1)\nElse\nAck = Ack(m - 1, Ack(m, n - 1))\nEnd If\nEnd Function\nresult = Ack(2, 3)" "result" (VInteger 9))
    "RGB and QBColor", (fun () ->
        expr "RGB(255, 0, 0)" (VInteger 255)
        expr "RGB(1, 2, 3)" (VInteger (1 + 2 * 256 + 3 * 65536))
        expr "QBColor(12)" (VInteger 0x0000FF))

    // ── 2. Error handling ──
    "Resume Next continues after the failing statement", (fun () ->
        vba "Dim result\nSub P()\nOn Error GoTo H\nresult = \"start\"\nresult = 1 / 0\nresult = \"after\"\nExit Sub\nH:\nResume Next\nEnd Sub\nP" "result" (VString "after"))
    "Resume retries the failing statement", (fun () ->
        vba "Dim result, d\nSub P()\nOn Error GoTo H\nd = 0\nresult = 10 / d\nExit Sub\nH:\nd = 2\nResume\nEnd Sub\nP" "result" (VDouble 5.0))
    "Resume label and Err cleared", (fun () ->
        vba "Dim result, n\nSub P()\nOn Error GoTo H\nErr.Raise 5\nresult = \"skipped\"\nDone:\nn = Err.Number\nExit Sub\nH:\nresult = \"handled\"\nResume Done\nEnd Sub\nP" "result" (VString "handled")
        vba "Dim result, n\nSub P()\nOn Error GoTo H\nErr.Raise 5\nDone:\nn = Err.Number\nExit Sub\nH:\nResume Done\nEnd Sub\nP" "n" (VInteger 0))
    "Resume without error is error 20", (fun () -> errNumber "Sub P()\nResume Next\nEnd Sub" "P" 20)
    "Run-time error numbers", (fun () ->
        errNumber "Dim x" "x = 1 / 0" 11
        errNumber "Dim x" "x = 5 \\ 0" 11
        errNumber "Dim x" "x = CInt(40000)" 6
        errNumber "Dim i As Integer" "i = 32767 + 1" 6
        errNumber "Dim a(2), x" "x = a(5)" 9
        errNumber "Dim a(2)" "a(-1) = 1" 9
        errNumber "Dim x" "x = \"a\" * 2" 13
        errNumber "Dim o" "Set o = Nothing\no.Foo" 91
        errNumber "Dim o, x" "x = o.Value" 91
        errNumber "Dim x" "x = 5\nx.Foo" 424
        errNumber "Class C\nPublic A\nEnd Class\nDim o" "Set o = New C\no.Missing" 438
        errNumber "Function F(a)\nEnd Function\nDim x" "x = F(1, 2)" 450
        errNumber "Function F(a, b)\nEnd Function\nDim x" "x = F(1)" 449
        errNumber "Dim x" "x = CreateObject(\"No.Such\")" 429
        errNumber "Dim x" "x = NoSuchProcedure(1)" 35
        errNumber "Dim x" "x = Null + Len(Null)\nx = CLng(Null)" 94)
    "Err object and Err.Source", (fun () ->
        expr "TypeName(Err)" (VString "ErrObject")
        vba "Dim n, s, d\nOn Error Resume Next\nErr.Raise vbObjectError + 1, \"MySource\", \"My message\"\nn = Err.Number - vbObjectError\ns = Err.Source\nd = Err.Description" "s" (VString "MySource")
        vba "Dim s\nOn Error Resume Next\nDim x\nx = 1 / 0\ns = Err.Source & \"|\" & Err.Description" "s" (VString "VBAProject|Division by zero")
        vba "Dim n\nOn Error Resume Next\nErr.Number = 7\nn = Err\nErr.Clear\nn = n + Err.Number" "n" (VInteger 7)
        vba "Dim n\nOn Error Resume Next\nErr.Raise 13\nIf Err Then n = Err.Description" "n" (VString "Type mismatch"))
    "Runtime errors carry the source line", (fun () ->
        let message = runtimeFailure RunOptions.vba "Dim x\nSub Inner()\nx = 1\nx = x / 0\nEnd Sub\nInner"
        contains message "'11'"
        contains message "line 4"
        contains message "Inner")
    "On Error GoTo -1 allows a second handler", (fun () ->
        vba "Dim result\nOn Error GoTo H1\nErr.Raise 5\nH1:\nOn Error GoTo -1\nOn Error GoTo H2\nErr.Raise 6\nresult = 0\nGoTo Done\nH2:\nresult = Err.Number\nDone:" "result" (VInteger 6))

    // ── 3. Declared types ──
    "Declared types start at their defaults", (fun () ->
        let source = "Dim i As Integer, l As Long, d As Double, s As String, b As Boolean, v, o As Object, r1, r2, r3\nr1 = TypeName(i) & TypeName(l) & TypeName(d) & TypeName(s) & TypeName(b) & TypeName(v)\nr2 = (o Is Nothing)\nFunction F() As String\nEnd Function\nr3 = TypeName(F())"
        vba source "r1" (VString "IntegerIntegerDoubleStringBooleanEmpty")
        vba source "r2" (VBoolean true)
        vba source "r3" (VString "String"))
    "Assignments convert to the declared type", (fun () ->
        let source = "Dim i As Integer, j As Long, k As Long, s As String, b As Boolean, d As Double\ni = 7.6\nj = 6.5\nk = \"12\"\ns = 3.5\nb = 1\nd = 2"
        vba source "i" (VInteger 8)
        vba source "j" (VInteger 6)
        vba source "k" (VInteger 12)
        vba source "s" (VString "3.5")
        vba source "b" (VBoolean true)
        vba source "d" (VDouble 2.0))
    "ReDim with an As clause", (fun () ->
        vba "Dim a() As Long, result\nReDim a(3) As Long\nresult = a(2) + UBound(a)" "result" (VInteger 3))
    "Typed parameters and locals", (fun () ->
        vba "Dim result\nFunction F(ByVal n As Integer) As Long\nDim t As Long\nt = n\nF = t * 2\nEnd Function\nresult = F(2.5)" "result" (VInteger 4))
    "Integer and Long overflow raise error 6", (fun () ->
        errNumber "Dim l As Long" "l = 2147483647\nl = l + 1" 6
        errNumber "Dim i As Integer" "i = 32767\ni = i + 1" 6
        errNumber "Dim i As Integer" "For i = 32760 To 40000\nNext" 6
        vba "Dim x\nx = 2147483647\nx = x + 1" "x" (VDouble 2147483648.0))

    // ── 4. Host bridge ──
    "Default-member assignment on host objects", (fun () ->
        let sheet = Sheet()
        sheetRun sheet "Range(\"A1\") = 5\nCells(2, 3) = \"x\"\nActiveSheet.Range(\"B1\") = 9\nSet r = Range(\"C1\")\nr.Value = 4\nWith ActiveSheet\n.Range(\"D1\") = 10\nEnd With"
        expectCell sheet "A1" (VInteger 5)
        expectCell sheet "R2C3" (VString "x")
        expectCell sheet "B1" (VInteger 9)
        expectCell sheet "C1" (VInteger 4)
        expectCell sheet "D1" (VInteger 10))
    "Host objects read through their default member", (fun () ->
        let sheet = Sheet()
        sheet.Cells.["A1"] <- VInteger 41
        sheet.Cells.["R1C1"] <- VString "x"
        sheet.Cells.["R1C2"] <- VString "yz"
        let result = run (sheetOptions sheet) "Dim a, b, c, d, e, f, g, r\na = Range(\"A1\")\nb = Range(\"A1\") + 1\nIf Cells(1, 1) = \"x\" Then c = True\nd = d & Cells(1, 2) & Cells(1, 1)\ne = Len(Cells(1, 2))\nSet r = Range(\"A1\")\nf = TypeName(r)\ng = CDbl(Range(\"A1\")) / 2\nMsgBox Range(\"A1\")"
        for name, expected in [ "a", VInteger 41; "b", VInteger 42; "c", VBoolean true; "d", VString "yzx"; "e", VInteger 2; "f", VString "Range"; "g", VDouble 20.5 ] do
            let actual = globalOf name result
            if actual <> expected then fail name expected actual)
    "Host calls as arguments to host methods", (fun () ->
        let sheet = Sheet()
        sheetRun sheet "Range(\"A1:B1\").Copy Range(\"D1\")\nRange(\"A2\").Copy(Range(\"D2\"))\nCall Range(\"A3\").Copy(Range(\"D3\"))\nActiveSheet.Range(\"A4\").Copy ActiveSheet.Range(\"D4\")\nSet d = Range(\"D5\")\nRange(\"A5\").Copy d"
        expectLog sheet [ "Copy A1:B1->D1"; "Copy A2->D2"; "Copy A3->D3"; "Copy A4->D4"; "Copy A5->D5" ])
    "Omitted and named arguments reach the host", (fun () ->
        let sheet = Sheet()
        sheetRun sheet "Dim a\na = Range(\"A1\").Address(, False)\nRange(\"A1\").Find What:=\"x\", LookAt:=2\nRange(\"A1\").Find \"y\", After:=xlUp"
        expectLog sheet [ "Address(missing,False)"; "Find What=x;LookAt=2"; "Find =y;After=-4162" ])
    "For Each over a host collection", (fun () ->
        vbaWith (sheetOptions (Sheet())) "Dim x, total\nFor Each x In ActiveSheet\ntotal = total + x\nNext" "total" (VInteger 6))
    "Unknown host members raise 438", (fun () ->
        vbaWith (sheetOptions (Sheet())) "Dim n\nOn Error Resume Next\nDim v\nv = ActiveSheet.NoSuchThing\nn = Err.Number" "n" (VInteger 438)
        vbaWith (sheetOptions (Sheet())) "Dim n\nOn Error Resume Next\nActiveSheet.NoSuchThing = 1\nn = Err.Number" "n" (VInteger 438)
        vbaWith (sheetOptions (Sheet())) "Dim n\nOn Error Resume Next\nActiveSheet.NoSuchMethod 1, 2\nn = Err.Number" "n" (VInteger 438))
    "Host errors carry their number", (fun () ->
        vbaWith (sheetOptions (Sheet())) "Dim n, d\nOn Error GoTo H\nRange(\"A1\").Fail\nExit Sub\nH:\nn = Err.Number\nd = Err.Description" "n" (VInteger 1004))
    "Host constants as plain globals", (fun () ->
        vbaWith (sheetOptions (Sheet())) "Dim n\nn = xlUp" "n" (VInteger -4162))

    // ── 5. Projects and sessions ──
    "Project lists procedures and macros", (fun () ->
        let project = projectWith defaultHooks
        let names = project |> Project.macros |> List.map (fun p -> p.QualifiedName)
        if names <> [ "Module1.Inc"; "Module1.Boom"; "Module1.Forever"; "Module1.Talk"; "Module2.UseOther"; "Module3.UseClass" ] then
            fail "macros" [] names
        let helper = project.Procedures |> List.find (fun p -> p.QualifiedName = "Module1.Helper")
        if helper.IsPublic || helper.Kind <> FunctionProcedure || helper.Line <> 10 || helper.EndLine <> 13 then fail "Helper" "private function at 10-13" helper
        let withArgs = project.Procedures |> List.find (fun p -> p.Name = "WithArgs")
        if withArgs.Parameters.Length <> 2 || not withArgs.Parameters.[1].Optional || withArgs.Parameters.[1].DefaultValue <> Some (VInteger 5) then
            fail "WithArgs" "two parameters" withArgs.Parameters
        let value = project.Procedures |> List.find (fun p -> p.Module = "Tally" && p.Name = "Value")
        if value.Kind <> PropertyGetProcedure || value.IsMacro then fail "Tally.Value" "property get" value)
    "Session calls procedures and keeps module state", (fun () ->
        let s = session (projectWith defaultHooks)
        finished "Inc" (s.Run("Module1.Inc", [])) VEmpty
        finished "Inc" (s.Run("Inc", [])) VEmpty
        if s.TryGetGlobal "Counter" <> Some (VInteger 2) then fail "Counter" 2 (s.TryGetGlobal "Counter")
        if s.TryGetGlobal "Module1.hidden" <> Some (VInteger 2) then fail "hidden" 2 (s.TryGetGlobal "Module1.hidden")
        finished "Twice" (s.Run("Module2.Twice", [ VInteger 21 ])) (VInteger 42)
        finished "UseOther" (s.Run("Module2.UseOther", [])) VEmpty
        if s.TryGetGlobal "Counter" <> Some (VInteger 4) then fail "Counter" 4 (s.TryGetGlobal "Counter")
        finished "WithArgs" (s.Run("WithArgs", [ VInteger 1 ])) VEmpty
        if s.TryGetGlobal "Counter" <> Some (VInteger 6) then fail "Counter default" 6 (s.TryGetGlobal "Counter")
        finished "UseClass" (s.Run("UseClass", [])) VEmpty
        if s.TryGetGlobal "Counter" <> Some (VInteger 9) then fail "Counter class" 9 (s.TryGetGlobal "Counter"))
    "Immediate window evaluates and executes", (fun () ->
        let s = session (projectWith defaultHooks)
        finished "?1+1" (s.RunImmediate "?1 + 1") (VInteger 2)
        finished "x = 5" (s.RunImmediate "x = 5 : y = x * 2") VEmpty
        finished "?x" (s.RunImmediate "? x + y") (VInteger 15)
        finished "call" (s.RunImmediate "Inc") VEmpty
        finished "?Counter" (s.RunImmediate "Print Counter") (VInteger 1)
        finished "?Twice" (s.RunImmediate "?Module2.Twice(4)") (VInteger 8)
        finished "module context" (s.RunImmediate("?Helper()", "Module2")) (VInteger 100)
        match s.RunImmediate "?1 +" with
        | Failed f when f.Kind = Syntax -> ()
        | other -> fail "syntax error" "Failed" other)
    "Session failures are structured", (fun () ->
        let s = session (projectWith defaultHooks)
        match s.Run("Module1.Boom", []) with
        | Failed f ->
            if f.Number <> 11 || f.Line <> 21 || f.Procedure <> "Boom" || f.Module <> "Module1" || f.Kind <> Runtime then fail "Boom" "11 at Module1.Boom line 21" f
        | other -> fail "Boom" "Failed" other
        // The session is usable after a failure.
        finished "after failure" (s.RunImmediate "?2 * 3") (VInteger 6)
        match s.Run("NoSuch", []) with
        | Failed f when f.Number = 35 -> ()
        | other -> fail "unknown procedure" "35" other)
    "Cooperative slices, location and cancellation", (fun () ->
        let s = session (projectWith defaultHooks)
        match s.Call("Forever", []) with
        | Error f -> fail "Call" "Ok" f
        | Ok () -> ()
        for _ in 1 .. 3 do
            if s.Step 1000 <> Running then fail "Step" Running s.Status
        match s.Location with
        | Some location when location.Procedure = "Forever" && location.Module = "Module1" && location.Line >= 24 && location.Line <= 27 -> ()
        | other -> fail "Location" "Forever" other
        match s.Call("Inc", []) with
        | Error f when f.Number = 17 -> ()
        | other -> fail "busy" "error 17" other
        s.Cancel()
        match s.Step 1000 with
        | Failed f when f.Number = 18 -> ()
        | other -> fail "Cancel" "error 18" other
        finished "after cancel" (s.Run("Module2.Twice", [ VInteger 1 ])) (VInteger 2)
        match s.Run("Forever", [], 5000) with
        | Running -> s.Cancel(); ignore (s.Step 1)
        | other -> fail "limit" Running other)
    "Hooks for output, dialogs and CreateObject", (fun () ->
        let output = ResizeArray<string>()
        let hooks = {
            Print = output.Add
            MsgBox = fun prompt buttons title -> output.Add(sprintf "MsgBox %s %d %s" prompt buttons title); 6
            InputBox = fun prompt title defaultText -> output.Add(sprintf "InputBox %s %s %s" prompt title defaultText); Some "typed"
            CreateObject = fun progId ->
                if progId = "Fake.Thing" then
                    Some (VHostObject { new IHostObject with
                                          member _.TypeName = "Thing"
                                          member _.GetMember _ = None
                                          member _.SetMember _ _ = false
                                          member _.CallMethod _ _ = None
                                          member _.CallNamed _ _ _ = None
                                          member _.Enumerate () = None })
                else None }
        let s = session (projectWith hooks)
        finished "Talk" (s.Run("Talk", [])) VEmpty
        let expected = [ "a 1 b\t 2 "; "MsgBox Continue? 4 Title"; "InputBox Name? T default"; "typed"; "Thing" ]
        if List.ofSeq output <> expected then fail "hook output" expected (List.ofSeq output)
        if s.TryGetGlobal "Counter" <> Some (VInteger 6) then fail "MsgBox result" 6 (s.TryGetGlobal "Counter"))
    "Project syntax and compile errors are structured", (fun () ->
        match Project.compile ProjectOptions.vba [ ModuleSource.standard "Bad" "Sub A()\nx = 1 + * 2\nEnd Sub" ] with
        | Error [ f ] when f.Kind = Syntax && f.Line = 2 && f.Module = "Bad" -> ()
        | other -> fail "syntax" "line 2" other
        match Project.compile ProjectOptions.vba [ ModuleSource.standard "Bad" "Sub A()\nx = 1\nGoTo Nowhere\nEnd Sub" ] with
        | Error [ f ] when f.Kind = Compile && f.Line = 3 && f.Module = "Bad" -> ()
        | other -> fail "compile" "line 3" other)
    "Module header parsing", (fun () ->
        let header = ModuleSource.parseHeader counterClass
        if header.Name <> Some "Tally" then fail "VB_Name" "Tally" header.Name
        if (header.Body.Split('\n')).Length <> (counterClass.Split('\n')).Length then fail "line count" "same" header.Body
        if header.Body.Contains "Attribute" || header.Body.Contains "MultiUse" then fail "body" "stripped" header.Body)
    "Single-source project", (fun () ->
        match Project.ofSource ProjectOptions.vbScript "Dim total\nSub Add(n)\ntotal = total + n\nEnd Sub" with
        | Ok project ->
            let s = session project
            finished "Add" (s.Run("Add", [ VInteger 3 ])) VEmpty
            finished "Add" (s.Run("Add", [ VInteger 4 ])) VEmpty
            if s.TryGetGlobal "total" <> Some (VInteger 7) then fail "total" 7 (s.TryGetGlobal "total")
        | Error e -> fail "project" "Ok" e)
]

let runAll () =
    let mutable failures = []
    for name, test in cases do
        try
            test ()
            printfn "PASS %s" name
        with error ->
            failures <- (name, error.Message) :: failures
            printfn "FAIL %s: %s" name error.Message
    if not failures.IsEmpty then
        failwithf "%d of %d release groups failed:\n%s" failures.Length cases.Length (failures |> List.rev |> List.map (fun (name, error) -> name + ": " + error) |> String.concat "\n")
    printfn "Passed %d release groups" cases.Length
