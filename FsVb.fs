[<AutoOpen>]
module Rbt.Vb.Api

open Rbt.Vb
open Ast

let private equalsIgnoreCase (left: string) (right: string) =
#if FABLE_COMPILER
    let l = if isNull (box left) then "" else left.ToLowerInvariant()
    let r = if isNull (box right) then "" else right.ToLowerInvariant()
    l = r
#else
    System.String.Equals(left, right, System.StringComparison.OrdinalIgnoreCase)
#endif

type ParseOptions = {
    Dialect: Dialect
    Tolerant: bool
    HostGlobalNames: string list
}

type RunOptions = {
    Dialect: Dialect
    Tolerant: bool
    Validate: bool
    HostGlobals: Map<string, Values.Value>
}

type RunResult = {
    Program: Ast.Program
    Bytecode: Bytecode.BytecodeProgram
    State: StackVm.VmState
}

type RunError =
    | ParseError of string
    | ValidationErrors of Validator.ValidationError list
    | RuntimeError of string

module ParseOptions =
    let vbScript = { Dialect = VBScript; Tolerant = false; HostGlobalNames = [] }
    let vba = { Dialect = VBA; Tolerant = false; HostGlobalNames = [] }
    let tolerant dialect = { Dialect = dialect; Tolerant = true; HostGlobalNames = [] }
    let withHostGlobals names options = { options with HostGlobalNames = names }

module RunOptions =
    let vbScript = { Dialect = VBScript; Tolerant = false; Validate = true; HostGlobals = Map.empty }
    let vba = { Dialect = VBA; Tolerant = false; Validate = true; HostGlobals = Map.empty }
    let tolerant dialect = { Dialect = dialect; Tolerant = true; Validate = true; HostGlobals = Map.empty }
    let withHostGlobals globals options = { options with HostGlobals = globals }

let parseWith (options: ParseOptions) (source: string) : Result<Ast.Program, string> =
    if options.Tolerant then
        ParserSimple.parse_tolerant options.Dialect source
    else
        ParserSimple.parse_dialect options.Dialect source

let parse (source: string) : Result<Ast.Program, string> =
    parseWith ParseOptions.vbScript source

let validate (program: Ast.Program) : Validator.ValidationError list =
    Validator.validate program

let compile (program: Ast.Program) : Bytecode.BytecodeProgram =
    Compiler.compile program

let compileWithHostGlobals (names: string seq) (program: Ast.Program) : Bytecode.BytecodeProgram =
    Compiler.compileWithGlobalNames names program

let execute (bytecode: Bytecode.BytecodeProgram) : StackVm.VmState =
    StackVm.execute bytecode

let executeWithHostGlobals (globals: seq<string * Values.Value>) (bytecode: Bytecode.BytecodeProgram) : StackVm.VmState =
    StackVm.executeWithGlobals globals bytecode

let tryGetGlobal (name: string) (state: StackVm.VmState) =
    state.Program.GlobalNames
    |> Array.tryFindIndex (fun globalName ->
        equalsIgnoreCase globalName name)
    |> Option.bind (fun idx ->
        if idx >= 0 && idx < state.Globals.Length then Some state.Globals.[idx]
        else None)

let compileSource (options: ParseOptions) (source: string) : Result<Ast.Program * Bytecode.BytecodeProgram, RunError> =
    match parseWith options source with
    | Error err -> Error (ParseError err)
    | Ok program ->
        let errors = validate program
        if errors.Length > 0 then
            Error (ValidationErrors errors)
        else
            Ok (program, compileWithHostGlobals options.HostGlobalNames program)

let runSource (options: RunOptions) (source: string) : Result<RunResult, RunError> =
    let parseOptions = {
        Dialect = options.Dialect
        Tolerant = options.Tolerant
        HostGlobalNames = options.HostGlobals |> Map.toSeq |> Seq.map fst |> Seq.toList
    }
    match parseWith parseOptions source with
    | Error err -> Error (ParseError err)
    | Ok program ->
        let errors =
            if options.Validate then validate program
            else []
        if errors.Length > 0 then
            Error (ValidationErrors errors)
        else
            let bytecode = compileWithHostGlobals parseOptions.HostGlobalNames program
            try
                let state = executeWithHostGlobals (options.HostGlobals |> Map.toSeq) bytecode
                Ok {
                    Program = program
                    Bytecode = bytecode
                    State = state
                }
            with ex ->
                Error (RuntimeError ex.Message)
