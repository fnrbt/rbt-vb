module Rbt.Vb.Tests.PersistenceCases

#if !FABLE_COMPILER
open System
open Rbt.Vb
open Rbt.Vb.Api
open Rbt.Vb.Values
open Rbt.Vb.Bytecode

let private rejected name action =
    let mutable threw = false
    try action () with _ -> threw <- true
    if not threw then failwithf "%s: malformed bytecode accepted" name

let private compileSourceCode source =
    match compileSource ParseOptions.vbScript source with
    | Ok (_, program) -> program
    | Error error -> failwithf "Persistence fixture compilation failed: %A" error

let run () =
    let program = compileSourceCode "Dim result\nFunction Twice(n)\nTwice = n * 2\nEnd Function\nresult = Twice(21)"
    let bytes = BytecodeIO.serialize program
    let roundtrip = BytecodeIO.deserialize bytes
    let state = execute roundtrip
    if tryGetGlobal "result" state <> Some (VInteger 42) then failwith "Bytecode round-trip changed execution"
    let classProgram = compileSourceCode "Class C\nPrivate stored\nProperty Let P(v)\nstored = v\nEnd Property\nProperty Get P()\nP = stored\nEnd Property\nEnd Class\nDim o, result\nSet o = New C\no.P = 42\nresult = o.P"
    let classState = classProgram |> BytecodeIO.serialize |> BytecodeIO.deserialize |> execute
    if tryGetGlobal "result" classState <> Some (VInteger 42) then failwith "Property round-trip changed execution"
    let errorProgram = compileSourceCode "Dim result\nresult = 7\nOn Error Resume Next\nresult = 1 / 0"
    let errorState = errorProgram |> BytecodeIO.serialize |> BytecodeIO.deserialize |> execute
    if tryGetGlobal "result" errorState <> Some (VInteger 7) then failwith "Statement metadata did not survive serialization"
    if not (program.Code |> Array.exists (fun instruction -> instruction.LineNumber > 0)) then failwith "Source line metadata was lost"
    for count in [0; 1; 7; bytes.Length - 1] do
        rejected "truncated blob" (fun () -> BytecodeIO.deserialize bytes.[0 .. count - 1] |> ignore)
    let corrupt offset value =
        let result = Array.copy bytes
        Array.Copy(BitConverter.GetBytes(value: int), 0, result, offset, 4)
        result
    rejected "invalid magic" (fun () -> BytecodeIO.deserialize (corrupt 0 0) |> ignore)
    rejected "incompatible version" (fun () -> BytecodeIO.deserialize (corrupt 4 -1) |> ignore)
    rejected "oversized count" (fun () -> BytecodeIO.deserialize (corrupt 9 Int32.MaxValue) |> ignore)
    rejected "negative count" (fun () -> BytecodeIO.deserialize (corrupt 9 -1) |> ignore)
    let replaceFirst opcode =
        let code = Array.copy program.Code
        code.[0] <- { code.[0] with Opcode = opcode }
        { program with Code = code }
    for name, opcode in ["negative jump", Jump -1; "past-end jump", Jump Int32.MaxValue; "constant index", LoadConst Int32.MaxValue; "function index", Call (Int32.MaxValue, 0); "global index", LoadGlobal Int32.MaxValue; "negative locals", LoadLocal -1] do
        rejected name (fun () -> execute (replaceFirst opcode) |> ignore)
    rejected "global metadata mismatch" (fun () -> execute { program with Globals = -1 } |> ignore)
    if program.Functions.Length = 0 then failwith "Missing function fixture"
    let badFunctions = Array.copy program.Functions
    badFunctions.[0] <- { badFunctions.[0] with StartPC = Int32.MaxValue }
    rejected "function entrypoint" (fun () -> execute { program with Functions = badFunctions } |> ignore)
    printfn "PASS persistence round-trip and malformed bytecode boundaries"
#else
let run () = ()
#endif
