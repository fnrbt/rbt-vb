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
    expect "True coerces to minus one" "2 + True" (VDouble 1.0)
    expect "Null arithmetic propagates" "Null + 2" VNull
    expectRuntimeError "invalid numeric conversion" "2 * \"not a number\""
    expectRuntimeError "invalid Null conversion" "CDbl(Null)"
    expect "string ascending order" "\"a\" < \"b\"" (VBoolean true)
    expect "string equal bound" "\"a\" <= \"a\"" (VBoolean true)
    expect "string descending order" "\"b\" < \"a\"" (VBoolean false)
    expect "CInt midpoint rounds to even" "CInt(1.5)" (VInteger 2)
    expect "CInt negative midpoint rounds to even" "CInt(-2.5)" (VInteger -2)
    expect "CLng midpoint rounds to even" "CLng(2.5)" (VInteger 2)
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
    expect "InStr binary default" "InStr(\"Ab\", \"a\")" (VInteger 0)
    expect "InStr text comparison" "InStr(1, \"Ab\", \"a\", 1)" (VInteger 1)
    expectVbaText "InStr Option Compare Text default" "InStr(\"Ab\", \"a\")" (VInteger 1)
    expectVbaText "InStr uses Option Compare Text" "InStr(1, \"Ab\", \"a\", -1)" (VInteger 1)
    expectVbaText "InStr explicit binary overrides option" "InStr(1, \"Ab\", \"a\", 0)" (VInteger 0)
    expect "InStr at end" "InStr(3, \"abc\", \"c\")" (VInteger 3)
    expect "InStr beyond end" "InStr(5, \"abc\", \"a\")" (VInteger 0)
    expect "InStr empty needle" "InStr(2, \"abc\", \"\")" (VInteger 2)
    expectRuntimeError "InStr invalid start" "InStr(0, \"abc\", \"a\")"
    expect "Array upper bound" "UBound(Array(1, 2))" (VInteger 1)
    expect "Split then Join" "Join(Split(\"a,b\", \",\"), \"-\")" (VString "a-b")
