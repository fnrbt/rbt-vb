module Rbt.Vb.Cli

open Rbt.Vb
open Rbt.Vb.Api
open System
open Ast

let private printRunError = function
    | Api.ParseError err ->
        printfn "Parse error: %s" err
    | Api.ValidationErrors errors ->
        printfn "Validation errors:"
        for err in errors do
            printfn "  - %s" err.Message
    | Api.RuntimeError err ->
        printfn "Runtime error: %s" err

let runCode (dialect: Dialect) (tolerant: bool) (code: string) (label: string) =
    printfn "Executing: %s" label
    printfn "---"
    let options: Api.ParseOptions = {
        Dialect = dialect
        Tolerant = tolerant
        HostGlobalNames = []
    }
    match Api.compileSource options code with
    | Ok (_, bytecode) ->
        printfn "Parse successful!"
        printfn "Compiling to bytecode..."
        printfn "Bytecode: %d instructions, %d constants, %d functions"
            bytecode.Code.Length
            bytecode.Constants.Length
            bytecode.Functions.Length
        printfn "Running VM..."
        printfn "---"
        try
            Api.execute bytecode |> ignore
            printfn "---"
            printfn "VM execution complete"
            0
        with ex ->
            printRunError (Api.RuntimeError ex.Message)
            1
    | Error err ->
        printRunError err
        1

let printUsage () =
    printfn "VB Parser and Stack VM"
    printfn "Usage: Api.Cli [--vba] [--tolerant] <filename.vbs>"
    printfn "   or: Api.Cli [--vba] [--tolerant] -e \"VB code\""
    printfn ""
    printfn "  --vba       Parse as VBA (strict mode, rejects VBScript-only forms)"
    printfn "  --tolerant  Parse VBA superset, then validate against target dialect"

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printUsage ()
        1
    else
        let mutable dialect = VBScript
        let mutable tolerant = false
        let mutable args = argv |> Array.toList

        let rec parseFlags () =
            match args with
            | "--vba" :: rest ->
                dialect <- VBA
                args <- rest
                parseFlags ()
            | "--tolerant" :: rest ->
                tolerant <- true
                args <- rest
                parseFlags ()
            | _ -> ()
        parseFlags ()

        match args with
        | [] ->
            printUsage ()
            1
        | "-e" :: codeArgs when codeArgs.Length >= 1 ->
            let code = codeArgs |> String.concat " "
            runCode dialect tolerant code code
        | filename :: _ ->
            if IO.File.Exists filename then
                let code = IO.File.ReadAllText filename
                runCode dialect tolerant code filename
            else
                printfn "Error: File not found: %s" filename
                1
