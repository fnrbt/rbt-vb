module Rbt.Vb.Tests.RegressionCases

open Rbt.Vb
open Rbt.Vb.Api
open Rbt.Vb.Values

let private fail label expected actual =
    failwithf "%s: expected %A, received %A" label expected actual

let private run options source =
    match runSource options source with
    | Ok result -> result
    | Error error -> failwithf "Execution failed: %A\n%s" error source

let private globalValue name result =
    match tryGetGlobal name result.State with
    | Some value -> value
    | None -> failwithf "Missing global %s" name

let private checkGlobal options source name expected =
    let result = run options source
    let actual = globalValue name result
    if actual <> expected then fail name expected actual

let private checkNumber options source name expected =
    let result = run options source
    match globalValue name result with
    | VInteger n when float n = expected -> ()
    | VDouble n when abs (n - expected) < 0.00000001 -> ()
    | actual -> fail name (VDouble expected) actual

let private runtimeError options source =
    match runSource options source with
    | Error (RuntimeError _) -> ()
    | actual -> failwithf "Expected runtime error, received %A\n%s" actual source

let private parseError options source =
    match parseWith options source with
    | Error _ -> ()
    | Ok _ -> failwithf "Malformed source accepted: %s" source

let private script source name expected = checkGlobal RunOptions.vbScript source name expected
let private number source expected = checkNumber RunOptions.vbScript ("Dim result\nresult = " + source) "result" expected

type private Probe() =
    let calls = ResizeArray<Value array>()
    member _.Calls = calls
    interface IHostObject with
        member _.TypeName = "Probe"
        member _.GetMember _ = None
        member _.SetMember _ _ = false
        member _.CallMethod name args =
            if name = "Record" then
                calls.Add(Array.copy args)
                Some VEmpty
            else None

let private cases : (string * (unit -> unit)) list = [
    "C01 incomplete declaration rejects the whole input", (fun () ->
        for options in [ParseOptions.vbScript; ParseOptions.vba; ParseOptions.tolerant Ast.VBScript] do
            parseError options "x = 1\nDim\nx = 2")
    "C01 malformed blocks and trailing tokens fail", (fun () ->
        for source in ["If True Then\nx = 1"; "Sub Broken(\nx = 1"; "x = 1\nEnd If"; "x = 1\n@"] do
            parseError ParseOptions.vbScript source)
    "C01 valid trailing separators", (fun () -> script "Dim result\nresult = 9:\n' end\n" "result" (VInteger 9))
    "C02 arithmetic precedence and associativity", (fun () ->
        for expression, expected in ["2 * 3 ^ 2", 18.; "-2 ^ 2", -4.; "2 ^ 3 ^ 2", 64.; "(2 * 3) ^ 2", 36.; "7 \\ 2 * 2", 1.; "10 Mod 4 * 2", 2.] do
            number expression expected)
    "C02 concatenation and boolean precedence", (fun () ->
        script "Dim result\nresult = 1 + 2 & 3 + 4" "result" (VString "37")
        script "Dim result\nresult = Not 1 = 2" "result" (VBoolean true)
        script "Dim result\nresult = True Or True Xor True" "result" (VBoolean false))
    "C03 ElseIf chooses first matching branch", (fun () ->
        script "Dim result\nresult = 0\nIf False Then\nresult = 1\nElseIf True Then\nresult = 2\nElseIf True Then\nresult = 3\nElse\nresult = 4\nEnd If" "result" (VInteger 2))
    "C03 ElseIf reaches later and fallback branches", (fun () ->
        for second, expected in ["True", 3; "False", 4] do
            script ("Dim result\nIf False Then\nresult = 1\nElseIf False Then\nresult = 2\nElseIf " + second + " Then\nresult = 3\nElse\nresult = 4\nEnd If") "result" (VInteger expected))
    "C04 inline If retains all statements", (fun () ->
        script "Dim result\nresult = 0\nIf False Then result = 1 : result = 2\n" "result" (VInteger 0)
        script "Dim result\nresult = 0\nIf False Then result = 1 : result = 2 Else result = 3 : result = result + 1\n" "result" (VInteger 4))
    "C05 unconditional Do repeats until exit", (fun () ->
        script "Dim result\nresult = 0\nDo\nresult = result + 1\nIf result = 3 Then Exit Do\nLoop" "result" (VInteger 3))
    "C05 pre and post tested Do loops", (fun () ->
        script "Dim result\nresult = 0\nDo While False\nresult = 99\nLoop\nDo\nresult = result + 1\nLoop Until result = 3" "result" (VInteger 3))
    "C06 forward and mutual procedure calls", (fun () ->
        script "Dim result\nSub First()\nCall Second()\nEnd Sub\nSub Second()\nresult = 42\nEnd Sub\nCall First()" "result" (VInteger 42)
        script "Dim result\nFunction EvenN(n)\nIf n = 0 Then\nEvenN = True\nElse\nEvenN = OddN(n - 1)\nEnd If\nEnd Function\nFunction OddN(n)\nIf n = 0 Then\nOddN = False\nElse\nOddN = EvenN(n - 1)\nEnd If\nEnd Function\nresult = EvenN(10)" "result" (VBoolean true))
    "C06 recursive ByRef forwarding", (fun () ->
        script "Dim result\nSub Count(ByRef value, ByVal n)\nIf n = 0 Then Exit Sub\nvalue = value + 1\nCall Count(value, n - 1)\nEnd Sub\nresult = 0\nCall Count(result, 4)" "result" (VInteger 4))
    "C06 forward class construction", (fun () ->
        script "Dim result, obj\nFunction Make()\nSet Make = New Box\nEnd Function\nClass Box\nPublic Value\nSub Class_Initialize()\nValue = 12\nEnd Sub\nEnd Class\nSet obj = Make()\nresult = obj.Value" "result" (VInteger 12))
    "C06 unknown call is an error", (fun () -> runtimeError RunOptions.vbScript "Call MissingProcedure()")
    "C07 class fields do not leak into globals", (fun () ->
        script "Class C\nPublic x\nSub M()\nEnd Sub\nEnd Class\nDim x\nx = 7" "x" (VInteger 7))
    "C07 ordinary locals shadow class fields and globals", (fun () ->
        script "Class C\nPublic x\nSub M()\nEnd Sub\nEnd Class\nDim result, x\nx = 8\nFunction F()\nDim x\nx = 3\nF = x\nEnd Function\nresult = F() + x" "result" (VInteger 11))
    "C08 property assignment has no stack result", (fun () ->
        let result = run RunOptions.vbScript "Class C\nPrivate stored\nProperty Let P(v)\nstored = v\nEnd Property\nProperty Get P()\nP = stored\nEnd Property\nEnd Class\nDim o, i, result\nSet o = New C\nFor i = 1 To 70000\no.P = i\nNext\nresult = o.P"
        if globalValue "result" result <> VInteger 70000 then failwith "Property result lost"
        if result.State.Sp <> 0 then failwithf "Property assignment leaked %d operands" result.State.Sp)
    "C09 Resume Next preserves failed assignment target", (fun () ->
        script "Dim result, nextValue\nresult = 7\nOn Error Resume Next\nresult = 1 / 0 : nextValue = 9" "result" (VInteger 7)
        script "Dim result\nOn Error Resume Next\nresult = 1 / 0 : result = 9" "result" (VInteger 9))
    "C09 repeated errors preserve stack baseline", (fun () ->
        let result = run RunOptions.vbScript "Dim i, result\nresult = 7\nOn Error Resume Next\nFor i = 1 To 1000\nresult = 1 / 0\nNext"
        if globalValue "result" result <> VInteger 7 || result.State.Sp <> 0 then failwith "Error recovery corrupted assignment or stack")
    "C10 callee handler does not escape its frame", (fun () ->
        runtimeError RunOptions.vbScript "Sub Quiet()\nOn Error Resume Next\nEnd Sub\nCall Quiet()\nDim result\nresult = 1 / 0")
    "C10 caller Resume Next unwinds failed call", (fun () ->
        script "Dim result\nSub Broken()\nDim x\nx = 1 / 0\nresult = 99\nEnd Sub\nOn Error Resume Next\nCall Broken()\nresult = 42" "result" (VInteger 42))
    "C10 caller label handler receives callee error", (fun () ->
        checkGlobal RunOptions.vba "Dim result\nSub Broken()\nDim x\nx = 1 / 0\nEnd Sub\nOn Error GoTo Handler\nCall Broken()\nresult = 99\nGoTo Done\nHandler:\nresult = 42\nDone:" "result" (VInteger 42))
    "C11 bare and inline implicit calls", (fun () ->
        script "Dim result\nSub Ping()\nresult = result + 1\nEnd Sub\nresult = 0\nPing\nIf True Then Ping\nIf False Then Ping" "result" (VInteger 2))
    "C11 C12 host calls preserve logical lines", (fun () ->
        let probe = Probe()
        let options = RunOptions.withHostGlobals (Map.ofList ["probe", VHostObject probe]) RunOptions.vbScript
        run options "If True Then probe.Record 1\nIf False Then probe.Record 99\nprobe.Record _\n 2" |> ignore
        let actual = probe.Calls |> Seq.map Array.toList |> Seq.toList
        if actual <> [[VInteger 1]; [VInteger 2]] then fail "host calls" [[VInteger 1]; [VInteger 2]] actual)
    "C12 expression continuation", (fun () -> script "Dim result\nresult = 1 + _\n 2" "result" (VInteger 3))
    "C13 runtime negative and fractional steps", (fun () ->
        script "Dim result, i, s\nresult = 0\ns = -1\nFor i = 3 To 1 Step s\nresult = result + i\nNext" "result" (VInteger 6)
        checkNumber RunOptions.vbScript "Dim result, i, s\nresult = 0\ns = -0.5\nFor i = 2 To 1 Step s\nresult = result + i\nNext" "result" 4.5)
    "C13 bounds and step evaluate once", (fun () ->
        script "Dim result, calls, i\ncalls = 0\nFunction Bound()\ncalls = calls + 1\nBound = 3\nEnd Function\nFunction StepSize()\ncalls = calls + 10\nStepSize = 1\nEnd Function\nFor i = 1 To Bound() Step StepSize()\nNext\nresult = calls" "result" (VInteger 11))
    "C14 global For Each variable", (fun () ->
        script "Dim x, result\nresult = 0\nFor Each x In Array(10, 20)\nresult = result + x\nNext" "result" (VInteger 30))
    "C15 nested For Each has independent state", (fun () ->
        script "Dim x, y, result\nresult = 0\nFor Each x In Array(1, 2)\nFor Each y In Array(3, 4)\nresult = result + x * y\nNext\nNext" "result" (VInteger 21))
    "C16 multidimensional cells remain independent", (fun () ->
        script "Dim a(1, 1), result\na(0, 0) = 10\na(0, 1) = 20\na(1, 0) = 30\nresult = a(0, 0) + a(0, 1) + a(1, 0)" "result" (VInteger 60))
    "C16 explicit lower bounds and dimension bounds", (fun () ->
        checkGlobal RunOptions.vba "Dim a(1 To 2, -1 To 1), result\na(2, -1) = 42\nresult = a(2, -1) + LBound(a, 1) + UBound(a, 2)" "result" (VInteger 44))
    "C16 multidimensional Preserve", (fun () ->
        checkGlobal RunOptions.vba "Dim a(), result\nReDim a(1 To 2, 1 To 2)\na(2, 2) = 42\nReDim Preserve a(1 To 2, 1 To 3)\nresult = a(2, 2)" "result" (VInteger 42))
    "C16 invalid indices and Preserve shape fail", (fun () ->
        for source in ["Dim a(1, 1), x\nx = a(0)"; "Dim a(1), x\nx = a(2)"; "Dim a(1)\na(-1) = 7"; "Dim a()\nReDim a(1, 1)\nReDim Preserve a(2, 1)"] do
            runtimeError RunOptions.vba source)
    "C17 method ByRef and ByVal", (fun () ->
        script "Class C\nSub Change(ByRef x, ByVal y)\nx = 9\ny = 99\nEnd Sub\nEnd Class\nDim o, x, y, result\nSet o = New C\nx = 1\ny = 2\nCall o.Change(x, y)\nresult = x + y" "result" (VInteger 11))
    "C17 parenthesized arguments force value", (fun () ->
        script "Dim result\nSub Change(ByRef x)\nx = 9\nEnd Sub\nresult = 1\nCall Change((result))" "result" (VInteger 1))
    "C17 array elements pass by reference", (fun () ->
        script "Dim a(1, 1), result\nSub Change(ByRef x)\nx = 9\nEnd Sub\na(0, 1) = 1\nCall Change(a(0, 1))\nresult = a(0, 1)" "result" (VInteger 9))
    "C18 Property Set uses its own handler", (fun () ->
        script "Class C\nPrivate stored\nProperty Set Child(v)\nSet stored = v\nEnd Property\nProperty Get Child()\nSet Child = stored\nEnd Property\nEnd Class\nDim o, child, result\nSet o = New C\nSet child = New C\nSet o.Child = child\nresult = o.Child Is child" "result" (VBoolean true))
    "C19 member and initializer casing", (fun () ->
        script "Class C\nPublic Value\nSub class_initialize()\nvalue = 7\nEnd Sub\nFunction ReadValue()\nReadValue = VALUE\nEnd Function\nEnd Class\nDim o, result\nSet o = New C\no.vAlUe = o.VALUE + 1\nresult = o.readvalue()" "result" (VInteger 8))
    "C20 Err.Raise transfers to label handler", (fun () ->
        checkGlobal RunOptions.vba "Dim result\nOn Error GoTo Handler\nErr.Raise 5, , \"failure\"\nresult = 99\nGoTo Done\nHandler:\nresult = Err.Number\nDone:" "result" (VInteger 5))
    "C20 Err.Raise resumes and retains description", (fun () ->
        script "Dim result\nOn Error Resume Next\nErr.Raise 5, , \"failure\"\nresult = Err.Description" "result" (VString "failure"))
    "C20 Err.Raise without handler fails", (fun () -> runtimeError RunOptions.vbScript "Err.Raise 5")
    "C21 coercion retains floating precision", (fun () -> number "2.5 * \"2\"" 5.; number "2.5 + Empty" 2.5; number "CDbl(True)" -1.)
    "C21 invalid numeric conversion fails", (fun () -> runtimeError RunOptions.vbScript "Dim result\nresult = CDbl(\"no number\")")
    "C21 Null propagates through arithmetic", (fun () -> script "Dim result\nresult = 2 + Null" "result" VNull)
    "C22 strings have relational ordering", (fun () ->
        for expression in ["\"a\" < \"b\""; "\"a\" <= \"a\""; "\"b\" > \"a\""; "\"b\" >= \"b\""] do
            script ("Dim result\nresult = " + expression) "result" (VBoolean true))
    "C22 Option Compare Text affects ordering", (fun () ->
        checkGlobal RunOptions.vba "Option Compare Text\nDim result\nresult = \"a\" = \"A\"" "result" (VBoolean true))
    "C23 conversion and rounding contracts", (fun () ->
        for expression, expected in ["CInt(1.5)", 2.; "CInt(2.5)", 2.; "CLng(-1.5)", -2.; "Int(-1.5)", -2.; "Fix(-1.5)", -1.; "Round(2.5)", 2.; "Round(-2.5)", -2.; "Round(1.25, 1)", 1.2] do
            number expression expected
        runtimeError RunOptions.vbScript "Dim result\nresult = CInt(32768)")
    "C24 StrComp respects compare argument", (fun () ->
        number "StrComp(\"a\", \"A\", 1)" 0.
        number "StrComp(\"a\", \"A\", 0)" 1.)
    "C24 InStr handles boundaries and comparison", (fun () ->
        for expression, expected in ["InStr(10, \"abc\", \"a\")", 0.; "InStr(1, \"abc\", \"B\", 1)", 2.; "InStr(1, \"abc\", \"B\", 0)", 0.; "InStr(1, \"\", \"x\")", 0.] do
            number expression expected)
    "C25 VBA-only expressions and case forms reject in VBScript", (fun () ->
        for source in ["Dim x\nx = TypeOf obj Is Foo"; "Select Case 2\nCase 1 To 3\nx = 1\nEnd Select"; "Select Case 2\nCase Is > 1\nx = 1\nEnd Select"] do
            for options in [RunOptions.vbScript; RunOptions.tolerant Ast.VBScript] do
                match runSource options source with
                | Error (ParseError _) | Error (ValidationErrors _) -> ()
                | _ -> failwithf "VBScript accepted VBA-only form: %s" source)
    "C25 VBA case ranges remain executable", (fun () ->
        checkGlobal RunOptions.vba "Dim result\nSelect Case 2\nCase 1 To 3\nresult = 42\nEnd Select" "result" (VInteger 42))
    "C17 object fields pass by reference", (fun () ->
        script "Class C\nPublic Value\nSub Change(ByRef x)\nx = 42\nEnd Sub\nEnd Class\nDim o, result\nSet o = New C\no.Value = 1\nCall o.Change(o.value)\nresult = o.VALUE" "result" (VInteger 42))
    "C21 Null condition does not enter branch", (fun () ->
        script "Dim result\nresult = 7\nIf Null Then result = 99" "result" (VInteger 7))
    "C10 active handler propagates to caller", (fun () ->
        checkGlobal RunOptions.vba "Dim result\nSub Inner()\nOn Error GoTo LocalHandler\nErr.Raise 5\nExit Sub\nLocalHandler:\nErr.Raise 6\nEnd Sub\nOn Error GoTo OuterHandler\nCall Inner()\nresult = 99\nGoTo Done\nOuterHandler:\nresult = Err.Number\nDone:" "result" (VInteger 6))
    "C14 local iteration shadows global", (fun () ->
        script "Dim x, result\nx = 99\nFunction Sum()\nDim x, total\ntotal = 0\nFor Each x In Array(1, 2, 3)\ntotal = total + x\nNext\nSum = total\nEnd Function\nresult = Sum() + x" "result" (VInteger 105))
    "C15 inner Exit For preserves outer loop", (fun () ->
        script "Dim x, y, result\nresult = 0\nFor Each x In Array(1, 2)\nFor Each y In Array(3, 4)\nresult = result + 1\nExit For\nNext\nNext" "result" (VInteger 2))
    "C17 method array-element references", (fun () ->
        script "Class C\nSub Change(ByRef x)\nx = 42\nEnd Sub\nEnd Class\nDim o, a(1, 1), result\nSet o = New C\nCall o.Change(a(1, 0))\nresult = a(1, 0)" "result" (VInteger 42))
    "C20 disabling handler restores error propagation", (fun () ->
        runtimeError RunOptions.vbScript "On Error Resume Next\nErr.Raise 5\nOn Error GoTo 0\nErr.Raise 6")
    "Integration recursive algorithm", (fun () ->
        script "Dim result\nFunction Fib(n)\nIf n < 2 Then\nFib = n\nElse\nFib = Fib(n - 1) + Fib(n - 2)\nEnd If\nEnd Function\nresult = Fib(10)" "result" (VInteger 55))
    "Integration array sort", (fun () ->
        script "Dim a, i, j, temp, result\na = Array(4, 1, 3, 2)\nFor i = 0 To UBound(a)\nFor j = 0 To UBound(a) - 1\nIf a(j) > a(j + 1) Then\ntemp = a(j)\na(j) = a(j + 1)\na(j + 1) = temp\nEnd If\nNext\nNext\nresult = Join(a, \",\")" "result" (VString "1,2,3,4"))
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
        failwithf "%d of %d regression groups failed:\n%s" failures.Length cases.Length (failures |> List.rev |> List.map (fun (name, error) -> name + ": " + error) |> String.concat "\n")
    printfn "Passed %d regression groups" cases.Length
