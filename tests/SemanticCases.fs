module Rbt.Vb.Tests.SemanticCases

open Rbt.Vb
open Rbt.Vb.Values

let private result label source =
    match Api.runSource Api.RunOptions.vbScript ("Dim x\nx = " + source) with
    | Ok execution ->
        match Api.tryGetGlobal "x" execution.State with
        | Some value -> value
        | None -> failwithf "%s: missing global x" label
    | Error error -> failwithf "%s: unexpected error %A" label error

let private expect label source expected =
    let actual = result label source
    if actual <> expected then failwithf "%s: expected %A, got %A" label expected actual

let private expectRuntimeError label source =
    match Api.runSource Api.RunOptions.vbScript ("Dim x\nx = " + source) with
    | Error (Api.RuntimeError _) -> ()
    | other -> failwithf "%s: expected runtime error, got %A" label other

let private expectVbaText label expression expected =
    match Api.runSource Api.RunOptions.vba ("Option Compare Text\nDim x\nx = " + expression) with
    | Ok execution ->
        match Api.tryGetGlobal "x" execution.State with
        | Some value when value = expected -> ()
        | actual -> failwithf "%s: expected %A, got %A" label expected actual
    | Error error -> failwithf "%s: unexpected error %A" label error

let run () =
    expect "numeric string multiplication" "2.5 * \"2\"" (VDouble 5.0)
    expect "Empty preserves fractional precision" "2.5 + Empty" (VDouble 2.5)
    expect "True coerces to minus one" "2 + True" (VInteger 1)
    expect "Null arithmetic propagates" "Null + 2" VNull
    expectRuntimeError "invalid numeric conversion" "2 * \"not a number\""
    expectRuntimeError "invalid Null conversion" "CDbl(Null)"
    expect "string ascending order" "\"a\" < \"b\"" (VBoolean true)
    expect "string equal bound" "\"a\" <= \"a\"" (VBoolean true)
    expect "string descending order" "\"b\" < \"a\"" (VBoolean false)
    expect "CInt midpoint rounds to even" "CInt(1.5)" (VInteger 2)
    expect "CInt negative midpoint rounds to even" "CInt(-2.5)" (VInteger -2)
    expect "CLng midpoint rounds to even" "CLng(2.5)" (VLong 2)
    expect "Int floors negatives" "Int(-1.5)" (VDouble -2.0)
    expect "Fix truncates negatives" "Fix(-1.5)" (VDouble -1.0)
    expect "Round ties to even" "Round(2.5)" (VDouble 2.0)
    expect "Round negative ties to even" "Round(-3.5)" (VDouble -4.0)
    expectRuntimeError "Round negative digits invalid" "Round(25, -1)"
    expectRuntimeError "CInt overflow" "CInt(32768)"
    expectRuntimeError "CLng overflow" "CLng(2147483648)"
    expect "StrComp binary default" "StrComp(\"A\", \"a\")" (VInteger -1)
    expect "StrComp explicit binary" "StrComp(\"A\", \"a\", 0)" (VInteger -1)
    expect "StrComp explicit text" "StrComp(\"A\", \"a\", 1)" (VInteger 0)
    expectRuntimeError "invalid StrComp mode" "StrComp(\"a\", \"a\", 2)"
    expect "StrComp use binary compare option" "StrComp(\"A\", \"a\", -1)" (VInteger -1)
    expectVbaText "StrComp Option Compare Text default" "StrComp(\"A\", \"a\")" (VInteger 0)
    expectVbaText "StrComp uses Option Compare Text" "StrComp(\"A\", \"a\", -1)" (VInteger 0)
    expectVbaText "StrComp explicit binary overrides option" "StrComp(\"A\", \"a\", 0)" (VInteger -1)
    expect "InStr binary default" "InStr(\"Ab\", \"a\")" (VLong 0)
    expect "InStr text comparison" "InStr(1, \"Ab\", \"a\", 1)" (VLong 1)
    expectVbaText "InStr Option Compare Text default" "InStr(\"Ab\", \"a\")" (VLong 1)
    expectVbaText "InStr uses Option Compare Text" "InStr(1, \"Ab\", \"a\", -1)" (VLong 1)
    expectVbaText "InStr explicit binary overrides option" "InStr(1, \"Ab\", \"a\", 0)" (VLong 0)
    expect "InStr at end" "InStr(3, \"abc\", \"c\")" (VLong 3)
    expect "InStr beyond end" "InStr(5, \"abc\", \"a\")" (VLong 0)
    expect "InStr empty needle" "InStr(2, \"abc\", \"\")" (VLong 2)
    expectRuntimeError "InStr invalid start" "InStr(0, \"abc\", \"a\")"
    expect "Array upper bound" "UBound(Array(1, 2))" (VLong 1)
    expect "Split then Join" "Join(Split(\"a,b\", \",\"), \"-\")" (VString "a-b")
    // 32-bit edges of Integer / Long arithmetic and subscripts (computed without
    // int64 since Portable.fs; a widened Variant result is the exact Double).
    let after setup expression = "0\n" + setup + "\nx = " + expression
    let vba label source =
        match Api.runSource Api.RunOptions.vba ("Dim x\n" + source) with
        | Ok execution -> Api.tryGetGlobal "x" execution.State |> Option.defaultValue VUndefined |> Ok
        | Error (Api.RuntimeError message) -> Error message
        | Error other -> failwithf "%s: %A" label other
    let expectVba label source expected =
        match vba label source with
        | Ok actual when actual = expected -> ()
        | other -> failwithf "%s: expected %A, got %A" label expected other
    let expectVbaError label source (number: int) =
        match vba label source with
        | Error message when message.Contains(sprintf "'%d'" number) -> ()
        | other -> failwithf "%s: expected error %d, got %A" label number other
    expect "Variant Long + widens" (after "a = 2147483647" "a + 1") (VDouble 2147483648.0)
    expect "Variant Long - widens" (after "a = CLng(-2147483647) - 1" "a - 1") (VDouble -2147483649.0)
    expect "most negative Long" (after "" "CLng(-2147483647) - CLng(1)") (VLong System.Int32.MinValue)
    expect "Variant Long * fits" (after "a = 46340" "a * a") (VLong 2147395600)
    expect "Variant Long * widens" (after "a = 46341" "a * a") (VDouble 2147488281.0)
    expect "Variant Long * widens exactly" (after "a = 2147483647" "a * a") (VDouble (2147483647.0 * 2147483647.0))
    expect "Long * Integer" (after "a = 2147483647" "a * -1") (VLong -2147483647)
    expect "Long Mod -1" (after "a = CLng(-2147483647) - 1" "a Mod -1") (VLong 0)
    expectRuntimeError "Long \\ -1 overflows" (after "a = CLng(-2147483647) - 1" "a \\ -1")
    expectRuntimeError "typed Long + overflows" "CLng(2147483647) + CLng(1)"
    expectRuntimeError "typed Long * overflows" "CLng(65536) * CLng(65536)"
    expect "Variant Integer + widens to Long" (after "a = CInt(32767)" "a + 1") (VLong 32768)
    expect "Variant Integer - widens to Long" (after "a = CInt(-32768)" "a - 1") (VLong -32769)
    expect "Variant Byte + widens to Integer" (after "a = CByte(200)" "a + a") (VInteger 400)
    expectVbaError "subscript at Long's top" "Dim arr(-2 To 2)\nx = arr(2147483647)" 9
    expectVbaError "subscript at Long's bottom" "Dim arr(-2 To 2)\nx = arr(-2147483647 - 1)" 9
    expectVbaError "subscript below the lower bound" "Dim arr(-2147483647 To -2147483640)\nx = arr(2147483647)" 9
    expectVba "subscript at the lower bound" "Dim arr(-2 To 2)\narr(-2) = 7\nx = arr(-2)" (VInteger 7)
    expectVba "subscript at the upper bound" "Dim arr(-2 To 2, 1 To 3)\narr(2, 3) = 9\nx = arr(2, 3)" (VInteger 9)
    expectVbaError "subscript past the upper bound" "Dim arr(-2 To 2, 1 To 3)\nx = arr(2, 4)" 9
    expectVbaError "Long overflow is error 6" "Dim a As Long\na = 2147483647\na = a + 1" 6
    expectVba "Long arithmetic at the edges" "Dim a As Long, b As Long\na = 2147483647\nb = -a - 1\nx = (a + b) * 3 - (b \\ 2)" (VLong 1073741821)
