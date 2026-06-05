open System
open Lexer
open ParserSimple
open Compiler
open StackVm
open Values
open Ast
open Validator

let runCode (dialect: Dialect) (tolerant: bool) (code: string) (label: string) =
    printfn "Executing: %s" label
    printfn "---"
    let parseResult =
        if tolerant then parse_tolerant dialect code
        else parse_dialect dialect code
    match parseResult with
    | Result.Ok program ->
        let errors = Validator.validate program
        if errors.Length > 0 then
            printfn "Validation errors:"
            for err in errors do
                printfn "  - %s" err.Message
            1
        else
            printfn "Parse successful!"
            printfn "Compiling to bytecode..."
            let bytecode = Compiler.compile program
            printfn "Bytecode: %d instructions, %d constants, %d functions"
                bytecode.Code.Length
                bytecode.Constants.Length
                bytecode.Functions.Length
            printfn "Running VM..."
            printfn "---"
            let finalState = StackVm.execute bytecode
            printfn "---"
            printfn "VM execution complete"
            0
    | Result.Error err ->
        printfn "Parse error: %s" err
        1

let printUsage () =
    printfn "VB Parser and Stack VM"
    printfn "Usage: VBScriptParser [--vba] [--tolerant] <filename.vbs>"
    printfn "   or: VBScriptParser [--vba] [--tolerant] -e \"VB code\""
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
            if System.IO.File.Exists filename then
                let code = System.IO.File.ReadAllText filename
                runCode dialect tolerant code filename
            else
                printfn "Error: File not found: %s" filename
                1
