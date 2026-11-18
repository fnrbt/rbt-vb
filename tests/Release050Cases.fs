module Rbt.Vb.Tests.Release050Cases

// 0.5.0 regressions: VBA's numeric subtypes (Integer, Long, Single, Byte),
// overflow of typed arithmetic, CStr formats, Like, Erl, Resume and the
// parser fixes. The parity group runs modules that also ran in Microsoft
// Excel 2019 and compares with what Excel produced. Shared by .NET and Fable.

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

/// A batch of `expression, expected` pairs evaluated in one VBA program.
let private exprs (setup: string) (cases: (string * Value) list) =
    let source =
        "Dim r(" + string (cases.Length) + ")\n" + setup + "\n" +
        (cases |> List.mapi (fun i (e, _) -> sprintf "r(%d) = %s" i e) |> String.concat "\n")
    let result = run RunOptions.vba source
    match globalOf "r" result with
    | VArray a ->
        cases |> List.iteri (fun i (e, expected) ->
            if a.Data.[i] <> expected then fail e expected a.Data.[i])
    | other -> fail "r" "array" other

let private str (s: string) = VString s

let private printed (modules: ModuleSource list) entry =
    let output = ResizeArray<string>()
    let options = ProjectOptions.vba |> ProjectOptions.withHooks { defaultHooks with Print = output.Add }
    match Project.compile options modules with
    | Error errors -> failwithf "Project failed: %A" errors
    | Ok p ->
        match Session.Create p with
        | Error f -> failwithf "Session failed: %A" f
        | Ok s ->
            match s.Run(entry, [], 1000000) with
            | Finished _ -> List.ofSeq output
            | other -> failwithf "%s: %A (output %A)" entry other (List.ofSeq output)

let private compileFailure (modules: ModuleSource list) =
    match Project.compile ProjectOptions.vba modules with
    | Error [ f ] -> f
    | other -> failwithf "expected one compile error, got %A" other

// ── A worksheet host for the parity modules: Worksheets(1).Cells(r, c).Value ──

type private ParityCell(cells: System.Collections.Generic.Dictionary<int * int, Value>, r: int, c: int) =
    inherit HostObject("Range")
    override _.SetMember name value =
        if name = "Value" || name = "" then cells.[(r, c)] <- value
        true
    override _.GetMember _ =
        match cells.TryGetValue((r, c)) with
        | true, v -> Some v
        | _ -> Some VEmpty

type private ParitySheet(cells: System.Collections.Generic.Dictionary<int * int, Value>) =
    inherit HostObject("Worksheet")
    override _.CallMethod name args =
        match name.ToLowerInvariant() with
        | "cells" -> Some (VHostObject (ParityCell(cells, ValueSemantics.toInteger args.[0], ValueSemantics.toInteger args.[1])))
        | "range" -> Some (VHostObject (ParityCell(cells, 1, 26)))
        | _ -> None

type private ParitySheets(cells) =
    inherit HostObject("Sheets")
    override _.CallMethod name _ = if name = "" || name = "Item" then Some (VHostObject (ParitySheet(cells))) else None

let private parity () =
    let failures = ResizeArray<string>()
    for name, source in ParityData.modules do
        let cells = System.Collections.Generic.Dictionary<int * int, Value>()
        let options =
            ProjectOptions.vba
            |> ProjectOptions.withHostGlobals (Map.ofList [ "Worksheets", VHostObject (ParitySheets(cells)) ])
            |> ProjectOptions.withHooks { defaultHooks with Print = ignore }
        match Project.compile options [ ModuleSource.standard "Module1" source ] with
        | Error errors -> failures.Add(sprintf "%s: %A" name errors)
        | Ok p ->
            match Session.Create p with
            | Error f -> failures.Add(sprintf "%s: %A" name f)
            | Ok s ->
                match s.Run("RbtParityMain", [], 20000000) with
                | Finished _ -> ()
                | other -> failures.Add(sprintf "%s: %A" name other)
                let text column row =
                    match cells.TryGetValue((row, column)) with
                    | true, VString t when t.StartsWith "'" -> t.Substring 1
                    | true, v -> ValueSemantics.toString v
                    | _ -> ""
                for case, row, typeName, cstr, error in ParityData.expected do
                    if case = name then
                        let actual = text 3 row, text 4 row, (let b = text 2 row in if b.StartsWith "#ERR" then b else "")
                        if actual <> (typeName, cstr, error) then
                            failures.Add(sprintf "%s row %d (%s): Excel %A, ours %A" name row (text 1 row) (typeName, cstr, error) actual)
    if failures.Count > 0 then failwith (String.concat "\n" failures)

let private groups : (string * (unit -> unit)) list = [
    "Excel parity probes", parity

    "Literals have their VBA types", (fun () ->
        exprs "" [
            "TypeName(5)", str "Integer"; "TypeName(70000)", str "Long"; "TypeName(-32768)", str "Long"
            "TypeName(5&)", str "Long"; "TypeName(5%)", str "Integer"; "TypeName(5!)", str "Single"
            "TypeName(5@)", str "Currency"; "TypeName(&H8000)", str "Integer"; "TypeName(&HFFFF&)", str "Long"
            "TypeName(3000000000)", str "Double"; "5@", VCurrency 50000L; "1.5!", single 1.5 ])

    "Typed arithmetic overflows; Variant arithmetic widens", (fun () ->
        exprs "Dim v, w, i As Integer, l As Long, n\nv = 30000: w = 30000\ni = 30000: l = 123456789\nOn Error Resume Next" [
            "v + w", VLong 60000
            "TypeName(CVar(123456789) * CVar(1000))", str "Double"
            "CVar(CByte(200)) + CVar(CByte(100))", VInteger 300
            "i + l", VLong 123486789
            "v + i", VLong 60000 ]
        for setup, statement in [
            "", "n = 30000 + 30000"; "", "n = 123456789 * 1000"; "Dim i As Integer: i = 30000", "n = i + i"
            "", "n = CInt(30000) + 30000"; "", "n = Len(\"ab\") * 70000 * 70000"; "Dim b As Byte: b = 200", "n = b + b"
            "Dim i As Integer: i = -32768", "n = -i"; "", "n = Abs(CInt(-32768))"; "Dim i As Integer", "For i = 32760 To 32767: Next"
            "", "n = CVar(-2147483647 - 1) \\ -1"; "", "n = 0 / 0"; "", "n = CSng(1E+38) * 10" ] do
            let result = run RunOptions.vba (sprintf "Dim n, r\n%s\nOn Error Resume Next\n%s\nr = Err.Number" setup statement)
            if globalOf "r" result <> VLong 6 then fail statement (VLong 6) (globalOf "r" result))

    "Result types of operators", (fun () ->
        exprs "Dim e" [
            "TypeName(CByte(1) + CByte(1))", str "Byte"; "TypeName(CByte(1) + 1)", str "Integer"
            "TypeName(CSng(1) + 1)", str "Single"; "TypeName(CSng(1) + 70000)", str "Double"
            "TypeName(CSng(1) / 3)", str "Single"; "TypeName(1 / 2)", str "Double"; "TypeName(7.5 \\ 2)", str "Long"
            "TypeName(CCur(1) * 1.5)", str "Double"; "TypeName(CCur(1) * 2)", str "Currency"
            "TypeName(True + True)", str "Integer"; "TypeName(e + CByte(1))", str "Byte"
            "Not CByte(1)", VByte 254uy; "TypeName(Not 1.5)", str "Long"; "3 Eqv 5", VInteger -7; "3 Imp 5", VInteger -3
            "TypeName(True And 1)", str "Integer"; "TypeName(Null Imp True)", str "Boolean"
            "TypeName(-CByte(1))", str "Integer"; "TypeName(CVar(CInt(-32768)) \\ -1)", str "Long" ])

    "Built-ins return Long, Integer, Single and Byte", (fun () ->
        exprs "Dim c As New Collection\nc.Add 1" [
            "TypeName(Len(\"a\"))", str "Long"; "TypeName(InStr(\"ab\", \"b\"))", str "Long"
            "TypeName(UBound(Array(1)))", str "Long"; "TypeName(Err.Number)", str "Long"; "TypeName(Erl)", str "Long"
            "TypeName(c.Count)", str "Long"; "TypeName(VarType(1))", str "Long"; "TypeName(vbRed)", str "Long"
            "TypeName(DateDiff(\"d\", #1/1/2020#, #1/2/2020#))", str "Long"; "TypeName(Year(#1/1/2020#))", str "Integer"
            "TypeName(Asc(\"A\"))", str "Integer"; "TypeName(AscB(\"A\"))", str "Byte"; "TypeName(Rnd)", str "Single"
            "TypeName(CSng(1))", str "Single"; "TypeName(CByte(1))", str "Byte"; "TypeName(Int(5&))", str "Long"
            "TypeName(Abs(CSng(-1)))", str "Single"; "TypeName(Sgn(-2.5))", str "Integer"
            "Year(#1/1/2020#) * 1000", VLong 2020000; "Hex(-1&)", str "FFFFFFFF"; "Hex(-1)", str "FFFF"
            "Hex(-1#)", str "FFFFFFFFFFFFFFFF"; "TypeName(DateDiff(\"s\", #1/1/1900#, #1/1/2020#))", str "Double" ])

    "Variant For counters take the widest type", (fun () ->
        exprs "Dim k, j, n As Long\nn = 3\nFor k = 1 To n: Next\nFor j = 1 To 2 Step 0.5: Next" [
            "TypeName(k)", str "Long"; "TypeName(j)", str "Double" ])

    "Singles print with 7 digits", (fun () ->
        exprs "Dim s As Single\ns = 0.1" [
            "CStr(CSng(1) / 3)", str "0.3333333"; "CStr(s)", str "0.1"; "CStr(s * 3)", str "0.3"
            "CStr(CSng(123456789))", str "1.234568E+08"; "Str(CSng(1) / 3)", str " .3333333"
            "CStr(CDbl(s))", str "0.100000001490116"; "s = 0.1", VBoolean true
            "Format(CSng(2.675), \"0.00\")", str "2.68"; "Round(CSng(2.55), 1)", single 2.5 ])

    "Doubles print fixed up to 15 decimals", (fun () ->
        exprs "" [
            "CStr(0.000001)", str "0.000001"; "CStr(1E-10)", str "0.0000000001"; "CStr(1E-15)", str "0.000000000000001"
            "CStr(1.5E-15)", str "1.5E-15"; "CStr(1 / 3 / 1000)", str "3.33333333333333E-04"
            "CStr(1234567890123456#)", str "1.23456789012346E+15"; "Str(0.5)", str " .5"; "Str(-0.5)", str "-.5"
            "Str(True)", str "True"; "CStr(CCur(922337203685477#))", str "922337203685477" ])

    "Numbers read from text", (fun () ->
        exprs "" [
            "IsNumeric(\"$5\")", VBoolean true; "IsNumeric(\"(5)\")", VBoolean true; "IsNumeric(\"5-\")", VBoolean true
            "IsNumeric(\"1d2\")", VBoolean true; "IsNumeric(\"5%\")", VBoolean false; "IsNumeric(\",5\")", VBoolean false
            "CDbl(\"$1,234.50\")", VDouble 1234.5; "CDbl(\"(5)\")", VDouble -5.0; "CDbl(\"5-\")", VDouble -5.0
            "Val(\"&777\")", VDouble 511.0; "Val(\"1d2\")", VDouble 100.0; "Val(\"&HFFFF&\")", VDouble 65535.0 ])

    "Like: classes, ranges and case", (fun () ->
        exprs "" [
            "\"b7\" Like \"[a-c]#\"", VBoolean true; "\"2\" Like \"[1-3]\"", VBoolean true; "\"b\" Like \"[a-c]\"", VBoolean true
            "\"d7\" Like \"[!a-c]#\"", VBoolean true; "\"B\" Like \"[a-c]\"", VBoolean false; "\"a\" Like \"[]a\"", VBoolean true
            "\"a[\" Like \"a[[]\"", VBoolean true; "\"-\" Like \"[a-]\"", VBoolean true; "\"xxb7\" Like \"*[a-c]#\"", VBoolean true ]
        let text = run RunOptions.vba "Option Compare Text\nDim r\nr = \"B\" Like \"[a-c]\""
        if globalOf "r" text <> VBoolean true then fail "Option Compare Text Like" true (globalOf "r" text))

    "Erl is the line of the handling procedure", (fun () ->
        let lines =
            printed [ ModuleSource.standard "M"
                        ("Function Lines()\n    On Error GoTo h\n10  Dim a\n20  a = 1 / 0\n30  Lines = \"none\"\n    Exit Function\nh:\n    Lines = Erl\nEnd Function\n" +
                         "Function Last()\n    On Error GoTo h\n10  Dim a\n20  a = 1 / 0\n    Exit Function\nh:\n    Last = Erl\nEnd Function\n" +
                         "Function Caller()\n    On Error GoTo h\n10  Callee\n    Exit Function\nh:\n    Caller = Erl\nEnd Function\n" +
                         "Sub Callee()\n100 Dim z\n110 z = 1 / 0\nEnd Sub\n" +
                         "Function Cleared()\n    Dim a, r\n    On Error Resume Next\n10  a = 1 / 0\n    r = Erl\n    Err.Clear\n    Cleared = r & \",\" & Erl\nEnd Function\n" +
                         "Sub Main()\n    Debug.Print Lines(); Last(); Caller(); Cleared()\nEnd Sub\n") ] "Main"
        if lines <> [ " 10  20  10 10,10" ] then fail "Erl" [ " 10  20  10 10,10" ] lines)
]

let runAll () =
    let mutable failed = []
    for name, test in groups do
        try
            test ()
            printfn "PASS %s" name
        with error ->
            failed <- name :: failed
            eprintfn "FAIL %s: %s" name error.Message
    if not failed.IsEmpty then failwithf "%d of %d 0.5.0 groups failed: %s" failed.Length groups.Length (String.concat ", " (List.rev failed))
