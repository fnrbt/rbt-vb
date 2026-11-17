module Rbt.Vb.Tests.Release040Cases

// 0.4.0 regressions: dates, Format, Collection and Dictionary, built-ins,
// class-module features, Static/Erl/Option Explicit/Stop/End and Currency.
// Shared by .NET and Fable.

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

/// `result = <expression>` in VBA.
let private expr expression expected = vba ("Dim result\nresult = " + expression) "result" expected

/// A batch of `expression, expected` pairs evaluated in one program.
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

/// The Err.Number a statement raises.
let private errNumber setup statement expected =
    vba (sprintf "Dim result\n%s\nOn Error Resume Next\n%s\nresult = Err.Number" setup statement) "result" (VInteger expected)

let private date y m d = VDate (DateCore.fromComponents y m d 0.0)

let private printed (source: string) =
    let output = ResizeArray<string>()
    let options = RunOptions.vba |> RunOptions.withHooks { defaultHooks with Print = output.Add }
    run options source |> ignore
    List.ofSeq output

let private project (modules: ModuleSource list) =
    match Project.compile ProjectOptions.vba modules with
    | Ok p -> p
    | Error errors -> failwithf "Project failed: %A" errors

let private session (p: Project) =
    match Session.Create p with
    | Ok s -> s
    | Error failure -> failwithf "Session failed: %A" failure

let private finished label status =
    match status with
    | Finished _ -> ()
    | other -> fail label "Finished" other

let private globalIs (s: Session) name expected =
    match s.TryGetGlobal name with
    | Some v when v = expected -> ()
    | other -> fail name expected other

// ── A small host object with a default member ──

type private Cell(store: System.Collections.Generic.Dictionary<string, Value>, address: string) =
    inherit HostObject("Range")
    override _.GetMember name =
        match name with
        | "" | "Value" -> Some (match store.TryGetValue address with | true, v -> v | _ -> VEmpty)
        | "Address" -> Some (VString address)
        | _ -> None
    override _.SetMember name value =
        match name with
        | "" | "Value" -> store.[address] <- value; true
        | _ -> false

type private RangeFunction(store: System.Collections.Generic.Dictionary<string, Value>) =
    inherit HostObject("RangeFunction")
    override _.CallMethod name args =
        match name, args with
        | "", [| VString address |] -> Some (VHostObject (Cell(store, address)))
        | _ -> None

let private hostOptions (store: System.Collections.Generic.Dictionary<string, Value>) =
    RunOptions.vba |> RunOptions.withHostGlobals (Map.ofList [ "Range", VHostObject (RangeFunction store) ])

let private cases : (string * (unit -> unit)) list = [
    // ── 1. Dates ──
    "Date literals, TypeName and VarType", (fun () ->
        exprs "" [
            "#2024-01-31#", date 2024 1 31
            "#1/31/2024#", date 2024 1 31
            "TypeName(#1/31/2024#)", str "Date"
            "VarType(#1/31/2024#)", VInteger 7
            "CDbl(#1/31/2024#)", VDouble 45322.0
            "CDbl(#1/1/1900#)", VDouble 2.0
            "CStr(#1/31/2024 13:45#)", str "1/31/2024 1:45:00 PM"
            "CStr(#13:45:30#)", str "1:45:30 PM"
            "CStr(#12/30/1899#)", str "12:00:00 AM"
            "#1/31/2024 6:00:00 AM# = #2024-01-31 06:00#", VBoolean true ])
    "Date arithmetic and comparison", (fun () ->
        exprs "" [
            "#1/31/2024# + 1", date 2024 2 1
            "CStr(#1/31/2024# + 1)", str "2/1/2024"
            "1 + #1/31/2024#", date 2024 2 1
            "#2/1/2024# - #1/31/2024#", VDouble 1.0
            "TypeName(#2/1/2024# - #1/31/2024#)", str "Double"
            "#2/1/2024# - 1", date 2024 1 31
            "#1/31/2024# < #2/1/2024#", VBoolean true
            "#1/31/2024# = \"1/31/2024\"", VBoolean true
            "CStr(#1/31/2024# + 0.5)", str "1/31/2024 12:00:00 PM"
            "Int(#1/31/2024 13:00#)", date 2024 1 31 ])
    "DateSerial, TimeSerial and the date parts", (fun () ->
        exprs "Dim d\nd = #1/31/2024 13:45:10#" [
            "DateSerial(2024, 13, 1)", date 2025 1 1
            "DateSerial(2024, 3, 0)", date 2024 2 29
            "DateSerial(24, 1, 1)", date 2024 1 1
            "CStr(TimeSerial(13, 45, 30))", str "1:45:30 PM"
            "Year(d) & \"-\" & Month(d) & \"-\" & Day(d)", str "2024-1-31"
            "Hour(d) & \":\" & Minute(d) & \":\" & Second(d)", str "13:45:10"
            "Weekday(d)", VInteger 4
            "Weekday(d, vbMonday)", VInteger 3
            "WeekdayName(4)", str "Wednesday"
            "WeekdayName(1, True, vbMonday)", str "Mon"
            "MonthName(2)", str "February"
            "MonthName(2, True)", str "Feb"
            "Year(\"2020-06-15\")", VInteger 2020
            "IsNull(Year(Null))", VBoolean true ])
    "CDate, IsDate, DateValue, TimeValue", (fun () ->
        exprs "" [
            "CDate(\"2024-01-31\")", date 2024 1 31
            "CDate(\"Jan 31, 2024\")", date 2024 1 31
            "CDate(\"31 January 2024\")", date 2024 1 31
            "CDate(\"1/31/2024 1:45 PM\") = #1/31/2024 13:45#", VBoolean true
            "CDate(\"2024-01-31T13:45:00\") = #1/31/2024 13:45#", VBoolean true
            "CDate(45322)", date 2024 1 31
            "IsDate(\"1/31/2024\")", VBoolean true
            "IsDate(\"13:45\")", VBoolean true
            "IsDate(\"hello\")", VBoolean false
            "IsDate(#1/1/2024#)", VBoolean true
            "IsDate(\"2/30/2024\")", VBoolean false
            "DateValue(\"1/31/2024 13:45\")", date 2024 1 31
            "CStr(TimeValue(\"13:45\"))", str "1:45:00 PM" ]
        errNumber "Dim x" "x = CDate(\"not a date\")" 13)
    "DateAdd, DateDiff and DatePart", (fun () ->
        exprs "" [
            "DateAdd(\"m\", 1, #1/31/2024#)", date 2024 2 29
            "DateAdd(\"yyyy\", 1, #2/29/2024#)", date 2025 2 28
            "DateAdd(\"q\", 1, #1/15/2024#)", date 2024 4 15
            "DateAdd(\"d\", -1, #3/1/2024#)", date 2024 2 29
            "DateAdd(\"ww\", 2, #1/1/2024#)", date 2024 1 15
            "CStr(DateAdd(\"h\", 25, #1/1/2024#))", str "1/2/2024 1:00:00 AM"
            "CStr(DateAdd(\"n\", 90, #1/1/2024#))", str "1/1/2024 1:30:00 AM"
            "CStr(DateAdd(\"s\", 61, #1/1/2024#))", str "1/1/2024 12:01:01 AM"
            "DateDiff(\"d\", #1/1/2024#, #3/1/2024#)", VInteger 60
            "DateDiff(\"m\", #1/31/2024#, #2/1/2024#)", VInteger 1
            "DateDiff(\"yyyy\", #12/31/2023#, #1/1/2024#)", VInteger 1
            "DateDiff(\"q\", #3/31/2024#, #4/1/2024#)", VInteger 1
            "DateDiff(\"h\", #1/1/2024 23:59#, #1/2/2024 0:01#)", VInteger 1
            "DateDiff(\"n\", #1/1/2024 10:00#, #1/1/2024 11:30#)", VInteger 90
            "DateDiff(\"s\", #1/1/2024#, #1/1/2024 0:01#)", VInteger 60
            "DateDiff(\"ww\", #1/6/2024#, #1/7/2024#)", VInteger 1
            "DateDiff(\"w\", #1/1/2024#, #1/15/2024#)", VInteger 2
            "DateDiff(\"d\", #3/1/2024#, #1/1/2024#)", VInteger -60
            "DatePart(\"q\", #5/15/2024#)", VInteger 2
            "DatePart(\"y\", #2/1/2024#)", VInteger 32
            "DatePart(\"ww\", #1/1/2024#)", VInteger 1
            "DatePart(\"ww\", #12/31/2024#)", VInteger 53
            "DatePart(\"ww\", #1/1/2021#, vbMonday, vbFirstFourDays)", VInteger 53
            "DatePart(\"w\", #1/31/2024#)", VInteger 4
            "DatePart(\"h\", #1/31/2024 13:45#)", VInteger 13 ]
        errNumber "Dim x" "x = DateAdd(\"zz\", 1, #1/1/2024#)" 5)
    "Date variables, Now, Date and Time", (fun () ->
        exprs "Dim d As Date, e As Date, t\nd = \"1/31/2024\"\nt = Now\nDate = #1/1/2000#" [
            "d", date 2024 1 31
            "TypeName(e)", str "Date"
            "CStr(e)", str "12:00:00 AM"
            "TypeName(Now)", str "Date"
            "TypeName(Date)", str "Date"
            "TypeName(Time)", str "Date"
            "Int(t) = Date", VBoolean true
            "Date <= Now", VBoolean true
            "Time < 1", VBoolean true
            "Year(Date) >= 2024", VBoolean true ])
    "Hosts pass dates in and out", (fun () ->
        let options = RunOptions.vba |> RunOptions.withHostGlobals (Map.ofList [ "Due", VbDate.ofParts 2024 1 31 0 0 0 ])
        let result = run options "Dim later, kind\nlater = Due + 7\nkind = TypeName(Due)"
        if globalOf "kind" result <> VString "Date" then fail "kind" "Date" (globalOf "kind" result)
        match VbDate.tryToDateTime (globalOf "later" result) with
        | Some dt when dt.Year = 2024 && dt.Month = 2 && dt.Day = 7 -> ()
        | other -> fail "later" "2024-02-07" other
        match VbDate.ofDateTime (System.DateTime(2024, 1, 31, 13, 45, 0)) with
        | VDate serial when abs (serial - (45322.0 + 13.75 / 24.0)) < 1e-9 -> ()
        | other -> fail "ofDateTime" "45322.573" other)

    // ── 2. Format family ──
    "CStr and Debug.Print of Doubles use 15 digits", (fun () ->
        exprs "" [
            "CStr(0.1 + 0.2)", str "0.3"
            "CStr(1 / 3)", str "0.333333333333333"
            "CStr(2 ^ 0.5)", str "1.4142135623731"
            "CStr(1E+15)", str "1E+15"
            "CStr(123456789012345)", str "123456789012345"
            "CStr(0.0001)", str "0.0001"
            "CStr(0.00001)", str "1E-05"
            "CStr(-0.5)", str "-0.5"
            "CStr(1.5E+300)", str "1.5E+300"
            "\"x\" & 0.1 + 0.2", str "x0.3"
            "CStr(100)", str "100" ]
        let lines = printed "Debug.Print 0.1 + 0.2; -1.5; 1E-05"
        if lines <> [ " 0.3 -1.5  1E-05 " ] then fail "Debug.Print" [ " 0.3 -1.5  1E-05 " ] lines)
    "Format with numeric formats", (fun () ->
        exprs "" [
            "Format(1234.5, \"#,##0.00\")", str "1,234.50"
            "Format(0.5, \"0%\")", str "50%"
            "Format(-1234.5, \"#,##0.00;(#,##0.00)\")", str "(1,234.50)"
            "Format(-1234.5, \"#,##0.00\")", str "-1,234.50"
            "Format(0, \"0.00;-0.00;\"\"zero\"\"\")", str "zero"
            "Format(5551234567#, \"(000) 000-0000\")", str "(555) 123-4567"
            "Format(12345.678, \"0.00E+00\")", str "1.23E+04"
            "Format(0.000123, \"0.0E-00\")", str "1.2E-04"
            "Format(0.5, \"#.##\")", str ".5"
            "Format(7, \"000\")", str "007"
            "Format(2.5, \"0\")", str "3"
            "Format(1234567, \"#,##0,\")", str "1,235"
            "Format(3.14159, \"0.0#\")", str "3.14"
            "Format(3.1, \"0.0#\")", str "3.1"
            "Format(42, \"\\$0\")", str "$42"
            "Format(\"12.5\", \"0.00\")", str "12.50"
            "Format(1234.5)", str "1234.5"
            "Format(True)", str "True"
            "IsNull(Format(Null, \"0\"))", VBoolean true
            "Format(Empty)", str "" ])
    "Format with named formats", (fun () ->
        exprs "" [
            "Format(1234.5, \"Currency\")", str "$1,234.50"
            "Format(-5, \"Currency\")", str "($5.00)"
            "Format(1234.5, \"Fixed\")", str "1234.50"
            "Format(1234.5, \"Standard\")", str "1,234.50"
            "Format(0.125, \"Percent\")", str "12.50%"
            "Format(1234.5, \"Scientific\")", str "1.23E+03"
            "Format(0, \"Yes/No\")", str "No"
            "Format(2, \"True/False\")", str "True"
            "Format(-1, \"On/Off\")", str "On"
            "Format(1234.5, \"General Number\")", str "1234.5"
            "Format(#1/31/2024 13:05:09#, \"General Date\")", str "1/31/2024 1:05:09 PM"
            "Format(#1/31/2024#, \"Long Date\")", str "Wednesday, January 31, 2024"
            "Format(#1/31/2024#, \"Medium Date\")", str "31-Jan-24"
            "Format(#1/31/2024#, \"Short Date\")", str "1/31/2024"
            "Format(#13:05:09#, \"Long Time\")", str "1:05:09 PM"
            "Format(#13:05:09#, \"Medium Time\")", str "01:05 PM"
            "Format(#13:05:09#, \"Short Time\")", str "13:05" ])
    "Format with date and time tokens", (fun () ->
        exprs "Dim d\nd = #1/31/2024 13:05:09#" [
            "Format(d, \"yyyy-mm-dd hh:nn:ss\")", str "2024-01-31 13:05:09"
            "Format(d, \"dddd, mmmm d, yyyy\")", str "Wednesday, January 31, 2024"
            "Format(d, \"ddd mmm yy\")", str "Wed Jan 24"
            "Format(d, \"h:mm AM/PM\")", str "1:05 PM"
            "Format(d, \"hh:mm:ss\")", str "13:05:09"
            "Format(d, \"h:mm am/pm\")", str "1:05 pm"
            "Format(d, \"m/d/yy\")", str "1/31/24"
            "Format(d, \"mm/dd/yyyy\")", str "01/31/2024"
            "Format(d, \"q\")", str "1"
            "Format(d, \"y\")", str "31"
            "Format(d, \"w\")", str "4"
            "Format(d, \"\\Q\" & \"q yyyy\")", str "Q1 2024"
            "Format(d, \"\"\"Day\"\" d\")", str "Day 31"
            "Format(d, \"ddddd\")", str "1/31/2024"
            "Format(d, \"ttttt\")", str "1:05:09 PM"
            "Format(\"2024-01-31\", \"dd.mm.yyyy\")", str "31.01.2024"
            "Format(45322, \"yyyy-mm-dd\")", str "2024-01-31" ])
    "Format with string placeholders", (fun () ->
        exprs "" [
            "Format(\"abc\", \">\")", str "ABC"
            "Format(\"ABC\", \"<\")", str "abc"
            "Format(\"abc\", \"@@@@@\")", str "  abc"
            "Format(\"abc\", \"!@@@@@\")", str "abc  "
            "Format(\"abc\", \"&&&&&\")", str "abc"
            "Format(\"5551234\", \"@@@-@@@@\")", str "555-1234" ])
    "FormatNumber, FormatCurrency, FormatPercent, FormatDateTime", (fun () ->
        exprs "" [
            "FormatNumber(1234.567)", str "1,234.57"
            "FormatNumber(1234.567, 1)", str "1,234.6"
            "FormatNumber(0.5, 2, vbFalse)", str ".50"
            "FormatNumber(-5, 2, , vbTrue)", str "(5.00)"
            "FormatNumber(1234.4, 0, , , vbFalse)", str "1234"
            "FormatNumber(-0.001, 2)", str "0.00"
            "FormatCurrency(1234.5)", str "$1,234.50"
            "FormatCurrency(-5)", str "($5.00)"
            "FormatPercent(0.125)", str "12.50%"
            "FormatPercent(0.5, 0)", str "50%"
            "FormatDateTime(#1/31/2024 13:45#)", str "1/31/2024 1:45:00 PM"
            "FormatDateTime(#1/31/2024 13:45#, vbLongDate)", str "Wednesday, January 31, 2024"
            "FormatDateTime(#1/31/2024 13:45#, vbShortDate)", str "1/31/2024"
            "FormatDateTime(#1/31/2024 13:45#, vbLongTime)", str "1:45:00 PM"
            "FormatDateTime(#1/31/2024 13:45#, vbShortTime)", str "13:45" ])
    "Str and Val", (fun () ->
        exprs "" [
            "Str(5)", str " 5"
            "Str(-5)", str "-5"
            "Str(1.5)", str " 1.5"
            "Val(\"  12 34abc\")", VDouble 1234.0
            "Val(\"&HFF\")", VDouble 255.0
            "Val(\"&HFFFF\")", VDouble -1.0
            "Val(\"&O17\")", VDouble 15.0
            "Val(\"1e3\")", VDouble 1000.0
            "Val(\"abc\")", VDouble 0.0
            "Val(\"-3.5x\")", VDouble -3.5
            "Val(\".5\")", VDouble 0.5
            "Val(\"1,000\")", VDouble 1.0 ])

    // ── 3. Collection and Dictionary ──
    "Collection: Add with key, before and after; Item; Count; Remove", (fun () ->
        let setup = "Dim c As New Collection, s, x\nc.Add 1\nc.Add \"two\", \"k2\"\nc.Add 3, , 1\nc.Add 4, , , 1\nFor Each x In c\ns = s & x & \",\"\nNext"
        exprs setup [
            "s", str "3,4,1,two,"
            "c.Count", VInteger 4
            "c(1)", VInteger 3
            "c(\"K2\")", str "two"
            "c.Item(3)", VInteger 1
            "TypeName(c)", str "Collection" ]
        exprs "Dim c As Collection\nSet c = New Collection\nc.Add Item:=5, Key:=\"five\"\nc.Add 6, \"six\"\nc.Remove \"five\"\nc.Add 7, Before:=\"six\"\nc.Remove 2" [
            "c.Count", VInteger 1
            "c(1)", VInteger 7 ]
        errNumber "Dim c As New Collection" "c.Add 1, \"k\"\nc.Add 2, \"k\"" 457
        errNumber "Dim c As New Collection, x" "x = c(10)" 9
        errNumber "Dim c As New Collection, x" "c.Add 1\nx = c(\"nokey\")" 5
        errNumber "Dim c As New Collection" "c.Add 1\nc.Add 2, \"x\", 1, 1" 5
        errNumber "Dim c As New Collection" "c.Remove 1" 9)
    "Collection of objects", (fun () ->
        vba "Class Item\nPublic Name\nEnd Class\nDim c, result, o\nSet c = New Collection\nSet o = New Item\no.Name = \"first\"\nc.Add o\nc(1).Name = c(1).Name & \"!\"\nresult = o.Name & \"/\" & TypeName(c(1))" "result" (str "first!/Item"))
    "Scripting.Dictionary", (fun () ->
        let setup = "Dim d, s, k\nSet d = CreateObject(\"Scripting.Dictionary\")\nd.Add \"a\", 1\nd(\"b\") = 2\nd.Item(\"c\") = 3\nd(\"a\") = 10\nFor Each k In d\ns = s & k & \"=\" & d(k) & \";\"\nNext"
        exprs setup [
            "s", str "a=10;b=2;c=3;"
            "d.Count", VInteger 3
            "d.Exists(\"b\")", VBoolean true
            "d.Exists(\"B\")", VBoolean false
            "Join(d.Keys, \",\")", str "a,b,c"
            "Join(d.Items, \",\")", str "10,2,3"
            "TypeName(d)", str "Dictionary"
            "UBound(d.Keys)", VInteger 2 ]
        exprs "Dim d\nSet d = CreateObject(\"Scripting.Dictionary\")\nd.CompareMode = vbTextCompare\nd(\"Key\") = 1\nd(\"KEY\") = 2\nd.Add 1, \"one\"\nd.Key(\"key\") = \"renamed\"\nd.Remove 1#\nIf IsEmpty(d(\"missing\")) Then d.Remove \"missing\"" [
            "d.Count", VInteger 1
            "d(\"RENAMED\")", VInteger 2
            "d.Exists(1)", VBoolean false ]
        exprs "Dim d As New Dictionary, e As Object, n\nd.Add 1, \"x\"\nn = d.Exists(1#)\nSet e = New Scripting.Dictionary\nSet e(\"self\") = d\nd.RemoveAll" [
            "n", VBoolean true
            "d.Count", VInteger 0
            "e(\"self\").Count", VInteger 0 ]
        errNumber "Dim d\nSet d = CreateObject(\"Scripting.Dictionary\")" "d.Add \"a\", 1\nd.Add \"a\", 2" 457
        errNumber "Dim d\nSet d = CreateObject(\"Scripting.Dictionary\")" "d.Remove \"nope\"" 32811
        errNumber "Dim d" "Set d = CreateObject(\"Other.Thing\")" 429)

    // ── 4. Built-ins ──
    "IIf, Choose, Switch", (fun () ->
        exprs "" [
            "IIf(1 > 2, \"a\", \"b\")", str "b"
            "Choose(2, \"a\", \"b\", \"c\")", str "b"
            "IsNull(Choose(5, \"a\"))", VBoolean true
            "Switch(False, 1, True, 2)", VInteger 2
            "IsNull(Switch(False, 1))", VBoolean true ])
    "String built-ins with full argument lists", (fun () ->
        exprs "" [
            "StrConv(\"hello WORLD\", vbProperCase)", str "Hello World"
            "StrConv(\"abc\", vbUpperCase)", str "ABC"
            "StrConv(\"ABC\", vbLowerCase)", str "abc"
            "AscW(ChrW(8364))", VInteger 8364
            "LenB(\"abc\")", VInteger 6
            "Join(Filter(Array(\"apple\", \"banana\", \"cherry\"), \"an\"), \",\")", str "banana"
            "Join(Filter(Array(\"apple\", \"banana\", \"cherry\"), \"AN\", False, vbTextCompare), \",\")", str "apple,cherry"
            "InStrRev(\"abcabc\", \"bc\")", VInteger 5
            "InStrRev(\"abcabc\", \"bc\", 4)", VInteger 2
            "InStrRev(\"ABC\", \"b\", -1, vbTextCompare)", VInteger 2
            "InStrRev(\"abc\", \"x\")", VInteger 0
            "InStr(1, \"ABC\", \"b\", vbTextCompare)", VInteger 2
            "Replace(\"aaaa\", \"a\", \"b\", 2)", str "bbb"
            "Replace(\"aaaa\", \"a\", \"b\", 1, 2)", str "bbaa"
            "Replace(\"AbA\", \"a\", \"x\", , , vbTextCompare)", str "xbx"
            "UBound(Split(\"a,b,c,d\", \",\", 2))", VInteger 1
            "Split(\"a,b,c,d\", \",\", 2)(1)", str "b,c,d"
            "UBound(Split(\"aXbxc\", \"x\", -1, vbTextCompare))", VInteger 2
            "String(3, \"x\")", str "xxx"
            "String(2, 65)", str "AA"
            "Space(3)", str "   " ]
        errNumber "Dim x" "x = Asc(\"\")" 5
        errNumber "Dim x" "x = Mid(\"abc\", 0, 1)" 5)
    "Rnd and Randomize follow VBA's generator", (fun () ->
        let result = run RunOptions.vba "Dim a, b, c, d, e\na = Rnd(-1)\nb = Rnd\nc = Rnd(0)\nd = Rnd(-1)\nRandomize 5\ne = Rnd"
        let near name (expected: float) =
            match globalOf name result with
            | VDouble v when abs (v - expected) < 1e-7 -> ()
            | other -> fail name expected other
        near "a" 0.2240070104598999
        near "b" 0.035845816135406494
        near "c" 0.035845816135406494
        near "d" 0.2240070104598999
        match globalOf "e" result with
        | VDouble v when v >= 0.0 && v < 1.0 -> ()
        | other -> fail "e" "0 <= Rnd < 1" other)
    "Hex and Oct", (fun () ->
        exprs "" [
            "Hex(255)", str "FF"
            "Hex(-1)", str "FFFF"
            "Hex(-32769)", str "FFFF7FFF"
            "Hex(65536)", str "10000"
            "Oct(8)", str "10"
            "Oct(-1)", str "177777"
            "Hex(10.6)", str "B" ])
    "Error values and conversions", (fun () ->
        exprs "" [
            "TypeName(CVErr(2042))", str "Error"
            "VarType(CVErr(2042))", VInteger 10
            "IsError(CVErr(5))", VBoolean true
            "IsError(5)", VBoolean false
            "CStr(CVErr(5))", str "Error 5"
            "CDec(1.5)", VDouble 1.5
            "CVar(5)", VInteger 5
            "CSng(0.5)", VDouble 0.5
            "CLngLng(2 ^ 40)", VDouble 1099511627776.0
            "TypeName(CLngLng(5))", str "Integer"
            "VarType(Array(1))", VInteger 8204 ]
        errNumber "Dim x" "x = CByte(300)" 6)
    "Environ through the host", (fun () ->
        let options = RunOptions.vba |> RunOptions.withHooks { defaultHooks with Environ = fun name -> if name = "USERNAME" then "dave" else "" }
        let result = run options "Dim a, b\na = Environ(\"USERNAME\")\nb = Environ$(\"NOPE\")"
        if globalOf "a" result <> VString "dave" || globalOf "b" result <> VString "" then fail "Environ" "dave/\"\"" (globalOf "a" result))
    "Erase for every array kind", (fun () ->
        exprs "Type Pt\n  X As Long\nEnd Type\nDim a(3) As Long, b(), s(2) As String, p(1) As Pt, v(2)\na(1) = 5\nReDim b(5)\ns(0) = \"x\"\np(0).X = 9\nv(1) = \"y\"\nErase a, b, s, p, v" [
            "a(1)", VInteger 0
            "UBound(a)", VInteger 3
            "IsArray(b)", VBoolean true
            "s(0)", str ""
            "p(0).X", VInteger 0
            "IsEmpty(v(1))", VBoolean true ]
        errNumber "Dim b()\nReDim b(2)\nErase b" "Dim n\nn = UBound(b)" 9
        exprs "Dim z()\nReDim Preserve z(1)\nz(1) = 4" [ "z(1)", VInteger 4; "IsArray(Array())", VBoolean true ])
    "Mid, LSet and RSet statements", (fun () ->
        exprs "Dim s, t, u, w\ns = \"abcdef\"\nMid(s, 2, 2) = \"XYZ\"\nt = \"abcdef\"\nMid$(t, 5) = \"12345\"\nu = \"12345\"\nLSet u = \"ab\"\nw = \"12345\"\nRSet w = \"ab\"" [
            "s", str "aXYdef"
            "t", str "abcd12"
            "u", str "ab   "
            "w", str "   ab" ])
    "CallByName on VB and host objects", (fun () ->
        let store = System.Collections.Generic.Dictionary<string, Value>()
        let result =
            run (hostOptions store)
                ("Class Acc\nPrivate total\nPublic Property Get Value()\nValue = total\nEnd Property\nPublic Property Let Value(v)\ntotal = v\nEnd Property\n" +
                 "Public Function Add(n)\ntotal = total + n\nAdd = total\nEnd Function\nEnd Class\n" +
                 "Dim o, a, b\nSet o = New Acc\nCallByName o, \"Value\", VbLet, 5\na = CallByName(o, \"Add\", VbMethod, 3)\nb = CallByName(o, \"Value\", VbGet)\nCallByName Range(\"A1\"), \"Value\", VbLet, 7")
        if globalOf "a" result <> VInteger 8 || globalOf "b" result <> VInteger 8 then fail "CallByName" 8 (globalOf "b" result)
        if store.["A1"] <> VInteger 7 then fail "host CallByName" 7 store.["A1"])

    // ── 5. Class modules ──
    "Events: RaiseEvent reaches WithEvents handlers", (fun () ->
        let source =
            "Public Event Changed(ByVal n As Long, Cancel As Boolean)\n" +
            "Public Log As String\n" +
            "Public Sub Fire(n As Long)\n" +
            "    Dim c As Boolean\n" +
            "    RaiseEvent Changed(n, c)\n" +
            "    If c Then Log = Log & \"cancelled \" & n & \";\"\n" +
            "End Sub\n"
        let watcher =
            "Private WithEvents src As Source\n" +
            "Public Seen As Long\n" +
            "Public Sub Watch(s As Source)\n    Set src = s\nEnd Sub\n" +
            "Private Sub src_Changed(ByVal n As Long, Cancel As Boolean)\n    Seen = Seen + n\nEnd Sub\n"
        let main =
            "Private WithEvents src As Source\n" +
            "Public Total As Long\n" +
            "Public Result As String\n" +
            "Private Sub src_Changed(ByVal n As Long, Cancel As Boolean)\n    Total = Total + n\n    Cancel = (n > 5)\nEnd Sub\n" +
            "Public Sub Main()\n" +
            "    Dim w As New Watcher\n" +
            "    Set src = New Source\n" +
            "    w.Watch src\n" +
            "    src.Fire 3\n" +
            "    src.Fire 7\n" +
            "    Dim keep As Source\n" +
            "    Set keep = src\n" +
            "    Set src = Nothing\n" +
            "    keep.Fire 100\n" +
            "    Result = keep.Log & \"|\" & w.Seen\n" +
            "End Sub\n"
        let s = session (project [ ModuleSource.classModule "Source" source; ModuleSource.classModule "Watcher" watcher; ModuleSource.standard "Module1" main ])
        finished "Main" (s.Run("Main", []))
        globalIs s "Total" (VInteger 10)
        globalIs s "Result" (VString "cancelled 7;|110"))
    "Implements: calls through the interface", (fun () ->
        let shape = "Public Function Area() As Double\nEnd Function\nPublic Property Get Name() As String\nEnd Property\n"
        let square =
            "Implements IShape\nPrivate side As Double\nPublic Sub Init(s As Double)\n    side = s\nEnd Sub\n" +
            "Private Function IShape_Area() As Double\n    IShape_Area = side * side\nEnd Function\n" +
            "Private Property Get IShape_Name() As String\n    IShape_Name = \"square\"\nEnd Property\n"
        let main =
            "Public Result\nSub Main()\n    Dim sq As New Square, s As IShape\n    sq.Init 3\n    Set s = sq\n" +
            "    Result = s.Area & \" \" & s.Name & \" \" & (TypeOf s Is IShape) & \" \" & (TypeOf s Is Square)\nEnd Sub\n"
        let s = session (project [ ModuleSource.classModule "IShape" shape; ModuleSource.classModule "Square" square; ModuleSource.standard "Module1" main ])
        finished "Main" (s.Run("Main", []))
        globalIs s "Result" (VString "9 square True True"))
    "Dim As New creates the object on first use", (fun () ->
        exprs "Class Counter\nPublic N\nPrivate Sub Class_Initialize()\ninits = inits + 1\nEnd Sub\nEnd Class\nDim inits, c As New Counter, before, after\nbefore = inits\nc.N = 5\nafter = c.N\nSet c = Nothing\nc.N = c.N + 1" [
            "before", VEmpty
            "after", VInteger 5
            "c.N", VInteger 1
            "inits", VInteger 2 ])
    "Class_Terminate runs when the last reference goes", (fun () ->
        let cls = "Public Tag As String\nPrivate Sub Class_Terminate()\n    Log = Log & Tag & \";\"\nEnd Sub\n"
        let main =
            "Public Log As String\nPublic Step1 As String, Step2 As String, Step3 As String\n" +
            "Sub UseOne()\n    Dim t As New Tracked\n    t.Tag = \"local\"\nEnd Sub\n" +
            "Sub Main()\n    UseOne\n    Step1 = Log\n    Dim a As Tracked, b As Tracked\n    Set a = New Tracked\n    a.Tag = \"shared\"\n    Set b = a\n" +
            "    Set a = Nothing\n    Step2 = Log\n    Set b = Nothing\n    Step3 = Log\nEnd Sub\n"
        let s = session (project [ ModuleSource.classModule "Tracked" cls; ModuleSource.standard "Module1" main ])
        finished "Main" (s.Run("Main", []))
        globalIs s "Step1" (VString "local;")
        globalIs s "Step2" (VString "local;")
        globalIs s "Step3" (VString "local;shared;")
        let lines = printed "Class T\nPrivate Sub Class_Terminate()\nDebug.Print \"bye\"\nEnd Sub\nEnd Class\nDim g\nSet g = New T\nDebug.Print \"end\""
        if lines <> [ "end"; "bye" ] then fail "end of run" [ "end"; "bye" ] lines)
    "Default members of VB classes", (fun () ->
        let cls =
            "VERSION 1.0 CLASS\nBEGIN\n  MultiUse = -1\nEND\nAttribute VB_Name = \"Box\"\nPrivate v As Variant\n" +
            "Public Property Get Value() As Variant\nAttribute Value.VB_UserMemId = 0\n    Value = v\nEnd Property\n" +
            "Public Property Let Value(x As Variant)\n    v = x\nEnd Property\n"
        let list =
            "Private items(3)\nPublic Property Get Item(i As Long) As Variant\nAttribute Item.VB_UserMemId = 0\n    Item = items(i)\nEnd Property\n" +
            "Public Property Let Item(i As Long, x As Variant)\n    items(i) = x\nEnd Property\n"
        let main =
            "Public A, B, C, D, E\nSub Main()\n    Dim bx As New Box, l As New List3\n    bx = 5\n    A = bx\n    B = bx + 1\n    C = \"v=\" & bx\n" +
            "    l(2) = \"two\"\n    D = l(2)\n    If bx = 5 Then E = Len(bx)\nEnd Sub\n"
        let s = session (project [ ModuleSource.classModule "Box" cls; ModuleSource.classModule "List3" list; ModuleSource.standard "Module1" main ])
        finished "Main" (s.Run("Main", []))
        globalIs s "A" (VInteger 5)
        globalIs s "B" (VInteger 6)
        globalIs s "C" (VString "v=5")
        globalIs s "D" (VString "two")
        globalIs s "E" (VInteger 1))
    "Assignment through a host object's default member", (fun () ->
        let store = System.Collections.Generic.Dictionary<string, Value>()
        run (hostOptions store) "Dim r, o As Object\nSet r = Range(\"A1\")\nr = 5\nSet o = Range(\"B1\")\no = \"x\"\nr = r + 1" |> ignore
        if store.["A1"] <> VInteger 6 || store.["B1"] <> VString "x" then fail "default assignment" "6 / x" (store.["A1"], store.["B1"]))

    // ── 6. Statics, line numbers, Option Explicit, Stop, End ──
    "Static variables and Static procedures", (fun () ->
        exprs "Function Counter()\nStatic n As Long\nn = n + 1\nCounter = n\nEnd Function\nStatic Function Both()\nDim a, b(2)\na = a + 1\nb(1) = b(1) + 10\nBoth = a + b(1)\nEnd Function\nDim x, y\nx = Counter() + Counter()\nx = Counter()\ny = Both()\ny = Both()" [
            "x", VInteger 3
            "y", VInteger 22 ])
    "Numeric line labels and Erl", (fun () ->
        exprs "Dim r1, r2\nSub P()\n10 On Error GoTo H\n20 r2 = 1\n30 r2 = 1 / 0\n40 Exit Sub\nH:\nr1 = Erl\nResume 40\nEnd Sub\nP" [
            "r1", VInteger 30
            "r2", VInteger 1
            "Erl", VInteger 0 ]
        exprs "Dim n\nn = 0\n100 n = n + 1\nIf n < 3 Then GoTo 100" [ "n", VInteger 3 ])
    "Option Explicit is enforced when compiling", (fun () ->
        match runSource RunOptions.vba "Option Explicit\nDim x\nx = 1\ny = 2" with
        | Error (RunError.RuntimeError message) when message.Contains "Variable not defined: y" && message.Contains "line 4" -> ()
        | other -> fail "Option Explicit" "compile error at line 4" other
        match Project.compile ProjectOptions.vba [ ModuleSource.standard "M" "Option Explicit\nSub A()\n    Dim i As Long\n    For j = 1 To 2\n    Next\nEnd Sub" ] with
        | Error [ f ] when f.Kind = Compile && f.Line = 4 && f.Description.Contains "j" -> ()
        | other -> fail "project Option Explicit" "line 4" other
        // Declared names, ReDim, constants and host globals are fine.
        vba "Option Explicit\nDim result\nConst K = 2\nSub A()\nReDim arr(2)\narr(1) = K\nresult = arr(1) + Len(vbCrLf)\nEnd Sub\nA" "result" (VInteger 4))
    "Stop pauses a session and is a no-op otherwise", (fun () ->
        let s = session (project [ ModuleSource.standard "Module1" "Public X As Long\nSub Main()\n    X = 1\n    Stop\n    X = 2\nEnd Sub\n" ])
        match s.Run("Main", []) with
        | Paused location when location.Line = 4 && location.Procedure = "Main" -> ()
        | other -> fail "Stop" "Paused at line 4" other
        globalIs s "X" (VInteger 1)
        finished "resume" (s.RunToEnd())
        globalIs s "X" (VInteger 2)
        vba "Dim x\nx = 1\nStop\nx = 2" "x" (VInteger 2))
    "End stops the run and resets module state", (fun () ->
        let options = ProjectOptions.vba |> ProjectOptions.withHostGlobals (Map.ofList [ "Limit", VInteger 10 ])
        let p =
            match Project.compile options [ ModuleSource.standard "Module1" "Public Counter As Long\nPublic Seen\nSub Bump()\n    Counter = Counter + 1\nEnd Sub\nSub Quit()\n    Counter = 5\n    End\n    Counter = 6\nEnd Sub\nSub Check()\n    Seen = Limit + Counter\nEnd Sub\n" ] with
            | Ok p -> p
            | Error e -> failwithf "%A" e
        let s = session p
        finished "Bump" (s.Run("Bump", []))
        globalIs s "Counter" (VInteger 1)
        finished "Quit" (s.Run("Quit", []))
        globalIs s "Counter" (VInteger 0)
        finished "Check" (s.Run("Check", []))
        globalIs s "Seen" (VInteger 10)
        vba "Dim x\nx = 1\nEnd\nx = 2" "x" (VInteger 1))

    // ── 7. Currency ──
    "Currency keeps four decimals exactly", (fun () ->
        exprs "Dim c As Currency, total As Currency, i\nc = 0.1\nFor i = 1 To 10\ntotal = total + c\nNext" [
            "total = 1", VBoolean true
            "TypeName(total)", str "Currency"
            "VarType(c)", VInteger 6
            "CCur(1.23456)", VCurrency 12346L
            "CStr(CCur(0.1) + CCur(0.2))", str "0.3"
            "CCur(2.5) * 2", VCurrency 50000L
            "CCur(1.5) * CCur(1.5)", VCurrency 22500L
            "TypeName(CCur(1) + 1.5)", str "Currency"
            "TypeName(CCur(1) / 2)", str "Double"
            "CCur(\"12.345\")", VCurrency 123450L
            "Round(CCur(2.345), 2)", VCurrency 23400L
            "Format(CCur(1234.5), \"#,##0.00\")", str "1,234.50"
            "-CCur(1)", VCurrency -10000L
            "Abs(CCur(-1))", VCurrency 10000L ]
        errNumber "Dim c As Currency" "c = 1E+20" 6
        if ValueSemantics.toString (VCurrency 9223372036854775807L) <> "922337203685477.5807" then fail "max currency" "922337203685477.5807" (ValueSemantics.toString (VCurrency 9223372036854775807L))
        exprs "Dim c As Currency\nc = 922337203685477#" [ "c > 0", VBoolean true ])

    "Common macro idioms", (fun () ->
        exprs "Dim arr() As String, d As Object, k, n, parts\narr = Split(\"a,b,c\", \",\")\nSet d = CreateObject(\"Scripting.Dictionary\")\nFor Each k In Split(\"a b a c a\")\nIf d.Exists(k) Then d(k) = d(k) + 1 Else d.Add k, 1\nNext\nWith d\nn = .Count\nEnd With\nFunction Parts() As String()\nParts = Split(\"x y\")\nEnd Function\nparts = Parts()" [
            "UBound(arr)", VInteger 2
            "arr(1)", str "b"
            "d(\"a\")", VInteger 3
            "n", VInteger 3
            "UBound(parts)", VInteger 1
            "d.Keys()(1)", str "b" ]
        let lines = printed "Dim c As New Collection\nc.Add 5\nWith c\nDebug.Print .Count; .Item(1)\nEnd With\nDebug.Print c _\n    .Count"
        if lines <> [ " 1  5 "; " 1 " ] then fail "With member after Debug.Print" [ " 1  5 "; " 1 " ] lines)
    // ── 8. Host-visible API ──
    "Bytecode round-trips the new values and metadata", (fun () ->
#if FABLE_COMPILER
        ()
#else
        match compileSource ParseOptions.vba "Dim d, c\nd = #1/31/2024#\nc = CCur(1.5)" with
        | Ok (_, bytecode) ->
            let restored = BytecodeIO.deserialize (BytecodeIO.serialize bytecode)
            let state = executeWithHostGlobals Seq.empty restored
            if tryGetGlobal "d" state <> Some (date 2024 1 31) then fail "d" "date" (tryGetGlobal "d" state)
        | Error e -> fail "compile" "Ok" e
#endif
        )
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
        failwithf "%d of %d 0.4.0 groups failed:\n%s" failures.Length cases.Length (failures |> List.rev |> List.map (fun (name, error) -> name + ": " + error) |> String.concat "\n")
    printfn "Passed %d 0.4.0 groups" cases.Length
