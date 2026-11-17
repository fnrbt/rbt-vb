[<AutoOpen>]
module Rbt.Vb.Api

open Rbt.Vb
open Ast
open Values

let private equalsIgnoreCase (left: string) (right: string) =
#if FABLE_COMPILER
    let l = if isNull (box left) then "" else left.ToLowerInvariant()
    let r = if isNull (box right) then "" else right.ToLowerInvariant()
    l = r
#else
    System.String.Equals(left, right, System.StringComparison.OrdinalIgnoreCase)
#endif

// ─────────────────────────────────────────────────────────────────────────────
// Single-source API (0.1/0.2 compatible)
// ─────────────────────────────────────────────────────────────────────────────

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
    /// Debug.Print / MsgBox / InputBox / CreateObject callbacks.
    Hooks: RuntimeHooks
    /// Stop with an error after this many VM instructions.
    InstructionLimit: int option
}

type RunResult = {
    Program: Ast.Program
    Bytecode: Bytecode.BytecodeProgram
    State: StackVm.VmState
}

type RunError =
    | ParseError of string
    | ValidationErrors of Validator.ValidationError list
    /// The formatted run-time (or compile) error, e.g.
    /// "Run-time error '11': Division by zero (line 3 in Main)".
    | RuntimeError of string

/// Console-backed hooks: Debug.Print prints, MsgBox prints and returns vbOK,
/// InputBox reads a line (native) or returns its default (Fable), and
/// CreateObject raises error 429.
let defaultHooks : RuntimeHooks = Builtins.consoleHooks

module ParseOptions =
    let vbScript = { Dialect = VBScript; Tolerant = false; HostGlobalNames = [] }
    let vba = { Dialect = VBA; Tolerant = false; HostGlobalNames = [] }
    let tolerant dialect = { Dialect = dialect; Tolerant = true; HostGlobalNames = [] }
    let withHostGlobals names options = { options with HostGlobalNames = names }

module RunOptions =
    let vbScript = { Dialect = VBScript; Tolerant = false; Validate = true; HostGlobals = Map.empty; Hooks = defaultHooks; InstructionLimit = None }
    let vba = { vbScript with Dialect = VBA }
    let tolerant dialect = { vbScript with Dialect = dialect; Tolerant = true }
    let withHostGlobals globals (options: RunOptions) = { options with HostGlobals = globals }
    let withHooks hooks (options: RunOptions) = { options with Hooks = hooks }
    let withInstructionLimit limit (options: RunOptions) = { options with InstructionLimit = Some limit }

let parseWith (options: ParseOptions) (source: string) : Result<Ast.Program, string> =
    if options.Tolerant then
        ParserSimple.parse_tolerant options.Dialect source
    else
        ParserSimple.parse_dialect options.Dialect source

let parse (source: string) : Result<Ast.Program, string> =
    parseWith ParseOptions.vbScript source

let validate (program: Ast.Program) : Validator.ValidationError list =
    Validator.validate program

let private compileMessage (message: string) line =
    if line > 0 then sprintf "Compile error at line %d: %s" line message else "Compile error: " + message

let compile (program: Ast.Program) : Bytecode.BytecodeProgram =
    Compiler.compile program

let compileWithHostGlobals (names: string seq) (program: Ast.Program) : Bytecode.BytecodeProgram =
    Compiler.compileWithGlobalNames names program

/// Run a compiled program to completion with console hooks. An unhandled
/// error raises `StackVm.VbRuntimeException` (its Message is formatted).
let execute (bytecode: Bytecode.BytecodeProgram) : StackVm.VmState =
    StackVm.execute bytecode

let executeWithHostGlobals (globals: seq<string * Values.Value>) (bytecode: Bytecode.BytecodeProgram) : StackVm.VmState =
    StackVm.executeWithGlobals globals bytecode

/// Run a compiled program with hooks and an optional instruction limit.
let executeWith (hooks: RuntimeHooks) (instructionLimit: int option) (globals: seq<string * Values.Value>) (bytecode: Bytecode.BytecodeProgram) : StackVm.VmState =
    let state = StackVm.createState hooks globals bytecode
    match instructionLimit with
    | None -> StackVm.run state
    | Some limit ->
        if not (StackVm.runSlice state limit) then
            raise (StackVm.VbRuntimeException {
                Number = 17; Description = "Instruction limit exceeded"; Source = state.ProjectName
                Line = (let line, _, _ = StackVm.currentLocation state in line); Procedure = ""; Module = "" })
    state

let tryGetGlobal (name: string) (state: StackVm.VmState) =
    state.Program.GlobalNames
    |> Array.tryFindIndex (fun globalName ->
        equalsIgnoreCase globalName name)
    |> Option.bind (fun idx ->
        if idx >= 0 && idx < state.Globals.Length then Some (ValueSemantics.dereference state.Globals.[idx])
        else None)

let compileSource (options: ParseOptions) (source: string) : Result<Ast.Program * Bytecode.BytecodeProgram, RunError> =
    match parseWith options source with
    | Error err -> Error (ParseError err)
    | Ok program ->
        let errors = validate program
        if errors.Length > 0 then
            Error (ValidationErrors errors)
        else
            try
                Ok (program, compileWithHostGlobals options.HostGlobalNames program)
            with
            | Compiler.CompileFailure (message, line, _) -> Error (RuntimeError (compileMessage message line))
            | ex -> Error (RuntimeError ex.Message)

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
            try
                let bytecode = compileWithHostGlobals parseOptions.HostGlobalNames program
                let state = executeWith options.Hooks options.InstructionLimit (options.HostGlobals |> Map.toSeq) bytecode
                Ok {
                    Program = program
                    Bytecode = bytecode
                    State = state
                }
            with
            | Compiler.CompileFailure (message, line, _) -> Error (RuntimeError (compileMessage message line))
            | ex -> Error (RuntimeError ex.Message)

/// The text the Immediate window shows for a value.
let rec displayValue (value: Value) =
    match ValueSemantics.dereference value with
    | VNull -> "Null"
    | VEmpty -> ""
    | VNothing -> "Nothing"
    | VUndefined -> "Error 448"
    | VBoolean b -> if b then "True" else "False"
    | VArray _ -> "Variant()"
    | VObject o -> o.ClassName
    | VHostObject h ->
        match (try h.GetMember "" with _ -> None) with
        | Some (VHostObject _) | None -> h.TypeName
        | Some v -> displayValue v
    | v -> ValueSemantics.toString v

// ─────────────────────────────────────────────────────────────────────────────
// Projects, sessions and cooperative execution
// ─────────────────────────────────────────────────────────────────────────────

type VbErrorKind =
    | Syntax
    | Compile
    | Runtime

/// A syntax, compile or run-time error with its location. `Number` is the VB
/// error number for run-time errors and 0 otherwise.
type VbFailure = {
    Kind: VbErrorKind
    Number: int
    Description: string
    Source: string
    /// 1-based line in the module's source (0 when unknown).
    Line: int
    /// 1-based column for syntax errors (0 otherwise).
    Column: int
    /// Procedure name without the module ("" for module-level code).
    Procedure: string
    Module: string
}

module VbFailure =
    let format (failure: VbFailure) =
        let location =
            [ (if failure.Module <> "" then Some failure.Module else None)
              (if failure.Procedure <> "" then Some failure.Procedure else None) ]
            |> List.choose id |> String.concat "."
        let where =
            match failure.Line, location with
            | 0, "" -> ""
            | 0, loc -> " (" + loc + ")"
            | line, "" -> sprintf " (line %d)" line
            | line, loc -> sprintf " (%s, line %d)" loc line
        match failure.Kind with
        | Syntax -> "Syntax error: " + failure.Description + where
        | Compile -> "Compile error: " + failure.Description + where
        | Runtime -> sprintf "Run-time error '%d': %s%s" failure.Number failure.Description where

    let ofRuntime (f: StackVm.RuntimeFailure) =
        { Kind = Runtime; Number = f.Number; Description = f.Description; Source = f.Source
          Line = f.Line; Column = 0; Procedure = f.Procedure; Module = f.Module }

type ModuleKind =
    | Standard
    | Class
    | Document

type ModuleSource = {
    Name: string
    Kind: ModuleKind
    Source: string
}

/// The `Attribute` lines and export header of a module file.
type ModuleHeader = {
    /// `Attribute VB_Name = "..."`, when present.
    Name: string option
    Attributes: (string * string) list
    /// The source with header lines blanked out (line numbers are unchanged).
    Body: string
}

module ModuleSource =
    let standard name source = { Name = name; Kind = Standard; Source = source }
    let classModule name source = { Name = name; Kind = Class; Source = source }
    let document name source = { Name = name; Kind = Document; Source = source }

    /// Read and blank `VERSION`/`BEGIN … END` export headers and `Attribute`
    /// lines, as found in exported .bas/.cls files and in vbaProject parts.
    let parseHeader (source: string) : ModuleHeader =
        let lines = source.Replace("\r\n", "\n").Split('\n')
        let attributes = ResizeArray<string * string>()
        let mutable inBegin = false
        let mutable seenCode = false
        let starts (line: string) (prefix: string) =
            line.Length >= prefix.Length && equalsIgnoreCase (line.Substring(0, prefix.Length)) prefix
        for i in 0 .. lines.Length - 1 do
            let line = lines.[i].Trim()
            if not seenCode && starts line "VERSION " && line.ToUpperInvariant().Contains "CLASS" then
                lines.[i] <- ""
            elif not seenCode && (equalsIgnoreCase line "BEGIN" || starts line "BEGIN ") then
                inBegin <- true
                lines.[i] <- ""
            elif inBegin then
                if equalsIgnoreCase line "END" then inBegin <- false
                lines.[i] <- ""
            elif starts line "Attribute " then
                let text = line.Substring(10)
                match text.IndexOf '=' with
                | -1 -> attributes.Add((text.Trim(), ""))
                | eq ->
                    let value = text.Substring(eq + 1).Trim()
                    let value = if value.Length >= 2 && value.StartsWith "\"" && value.EndsWith "\"" then value.Substring(1, value.Length - 2) else value
                    attributes.Add((text.Substring(0, eq).Trim(), value))
                lines.[i] <- ""
            elif line <> "" then
                seenCode <- true
        let name =
            attributes |> Seq.tryFind (fun (k, _) -> equalsIgnoreCase k "VB_Name") |> Option.map snd
        { Name = name; Attributes = List.ofSeq attributes; Body = String.concat "\n" lines }

type ProcedureKind =
    | SubProcedure
    | FunctionProcedure
    | PropertyGetProcedure
    | PropertyLetProcedure
    | PropertySetProcedure

type ParameterInfo = {
    Name: string
    Optional: bool
    ByRef: bool
    IsParamArray: bool
    TypeName: string option
    /// The default of an Optional parameter, when it is a literal.
    DefaultValue: Value option
}

type ProcedureInfo = {
    Name: string
    Module: string
    ModuleKind: ModuleKind
    /// `Module.Name`, the name `Session.Call` accepts.
    QualifiedName: string
    Kind: ProcedureKind
    Parameters: ParameterInfo list
    IsPublic: bool
    /// 1-based line of the declaration and of its End line (0 when unknown).
    Line: int
    EndLine: int
}
with
    /// Shown in a macro picker: a public Sub without required parameters in a
    /// standard or document module.
    member this.IsMacro =
        this.Kind = SubProcedure && this.IsPublic && this.ModuleKind <> Class
        && this.Parameters |> List.forall (fun p -> p.Optional || p.IsParamArray)

type ProjectOptions = {
    Dialect: Dialect
    /// Host objects and constants, visible to every module. Plain values work
    /// as constants (e.g. "xlUp", VInteger -4162).
    HostGlobals: Map<string, Value>
    Hooks: RuntimeHooks
    /// Err.Source for engine errors.
    ProjectName: string
    Validate: bool
}

module ProjectOptions =
    let vba = { Dialect = VBA; HostGlobals = Map.empty; Hooks = defaultHooks; ProjectName = "VBAProject"; Validate = true }
    let vbScript = { vba with Dialect = VBScript }
    let withHostGlobals globals (options: ProjectOptions) = { options with HostGlobals = globals }
    let withHooks hooks (options: ProjectOptions) = { options with Hooks = hooks }
    let withProjectName name (options: ProjectOptions) = { options with ProjectName = name }

type Project = {
    Options: ProjectOptions
    Modules: ModuleSource list
    Procedures: ProcedureInfo list
    Bytecode: Bytecode.BytecodeProgram
    /// Parsed modules (header lines blanked).
    Programs: (ModuleSource * Ast.Program) list
}

type ExecutionStatus =
    /// Paused between slices; call `Step` again.
    | Running
    /// Finished; the value of a Function / expression, Empty otherwise.
    | Finished of Value
    | Failed of VbFailure

type Location = {
    Line: int
    Procedure: string
    Module: string
}

module private ProjectInternals =
    let toCompilerKind kind =
        match kind with
        | Standard -> Compiler.StandardModule
        | Class -> Compiler.ClassModule
        | Document -> Compiler.DocumentModule

    let literalValue expr =
        match expr with
        | Literal (Integer i) -> Some (VInteger i)
        | Literal (Double d) -> Some (VDouble d)
        | Literal (Ast.String s) -> Some (VString s)
        | Literal (Boolean b) -> Some (VBoolean b)
        | Literal Null -> Some VNull
        | Literal Empty -> Some VEmpty
        | Literal Nothing -> Some VNothing
        | Unary (Negate, Literal (Integer i)) -> Some (VInteger (-i))
        | Unary (Negate, Literal (Double d)) -> Some (VDouble (-d))
        | _ -> None

    let parameterInfo (p: Parameter) =
        { Name = p.Name; Optional = p.Optional; ByRef = p.ByRef; IsParamArray = p.IsParamArray
          TypeName = p.Type |> Option.map (fun t -> t.Name)
          DefaultValue = p.DefaultValue |> Option.bind literalValue }

    /// Declaration and End lines of procedures, found from the tokens.
    let procedureLines (source: string) =
        let found = ResizeArray<string * int * int>()   // lowercased name + kind suffix, line, end line
        let tokens = try Lexer.tokenize source |> List.toArray with _ -> [||]
        let mutable openIndex = -1
        let isLineStart i = i = 0 || tokens.[i].PrecedingNewline || tokens.[i - 1].Kind = Colon
        for i in 0 .. tokens.Length - 1 do
            if isLineStart i then
                let mutable j = i
                while j < tokens.Length && tokens.[j].Kind = Keyword &&
                      (match tokens.[j].Lexeme with "public" | "private" | "friend" | "static" | "global" | "default" -> true | _ -> false) do
                    j <- j + 1
                if j + 1 < tokens.Length && tokens.[j].Kind = Keyword then
                    match tokens.[j].Lexeme with
                    | "sub" | "function" ->
                        found.Add((tokens.[j + 1].Lexeme.ToLowerInvariant(), tokens.[i].Line, 0))
                        openIndex <- found.Count - 1
                    | "property" when j + 2 < tokens.Length ->
                        found.Add((tokens.[j + 2].Lexeme.ToLowerInvariant() + "_" + tokens.[j + 1].Lexeme, tokens.[i].Line, 0))
                        openIndex <- found.Count - 1
                    | "end" when openIndex >= 0 && j = i ->
                        match tokens.[j + 1].Lexeme with
                        | "sub" | "function" | "property" ->
                            let (n, l, _) = found.[openIndex]
                            found.[openIndex] <- (n, l, tokens.[j + 1].Line)
                            openIndex <- -1
                        | _ -> ()
                    | _ -> ()
        found

    let procedures (m: ModuleSource) (program: Ast.Program) =
        let lines = procedureLines (ModuleSource.parseHeader m.Source).Body
        let lineOf key =
            lines |> Seq.tryFind (fun (k, _, _) -> k = key) |> Option.map (fun (_, l, e) -> l, e) |> Option.defaultValue (0, 0)
        let isPublic mods = not (List.contains (VisibilityMod Private) mods)
        let make mods name kind (ps: Parameter list) key =
            let line, endLine = lineOf key
            { Name = name; Module = m.Name; ModuleKind = m.Kind
              QualifiedName = (if m.Name = "" then name else m.Name + "." + name)
              Kind = kind; Parameters = ps |> List.map parameterInfo; IsPublic = isPublic mods
              Line = line; EndLine = endLine }
        let ofFunction func =
            match func with
            | SubDecl (mods, name, ps, _) -> make mods name SubProcedure ps (name.ToLowerInvariant())
            | FunctionDecl (mods, name, ps, _, _) -> make mods name FunctionProcedure ps (name.ToLowerInvariant())
            | PropertyGet (mods, name, ps, _, _) -> make mods name PropertyGetProcedure ps (name.ToLowerInvariant() + "_get")
            | PropertyLet (mods, name, ps, _) -> make mods name PropertyLetProcedure ps (name.ToLowerInvariant() + "_let")
            | PropertySet (mods, name, ps, _) -> make mods name PropertySetProcedure ps (name.ToLowerInvariant() + "_set")
        program.TopLevels
        |> List.collect (function
            | FunctionDef f -> [ ofFunction f ]
            | ClassDecl (_, _, members) when m.Kind <> Class ->
                // A VBScript-style Class block inside a module.
                members |> List.choose (function FunctionDef f -> Some (ofFunction f) | _ -> None)
            | _ -> [])

    let parseModule (options: ProjectOptions) (m: ModuleSource) : Result<Ast.Program, VbFailure> =
        let header = ModuleSource.parseHeader m.Source
        match ParserSimple.parseDetailed options.Dialect header.Body with
        | Error (message, line, column) ->
            Error { Kind = Syntax; Number = 0; Description = message; Source = options.ProjectName
                    Line = line; Column = column; Procedure = ""; Module = m.Name }
        | Ok program ->
            let errors = if options.Validate then Validator.validate program else []
            match errors with
            | [] -> Ok program
            | first :: _ ->
                Error { Kind = Compile; Number = 0; Description = first.Message; Source = options.ProjectName
                        Line = 0; Column = 0; Procedure = ""; Module = m.Name }

    let compileFailure (options: ProjectOptions) message line moduleName =
        { Kind = Compile; Number = 0; Description = message; Source = options.ProjectName
          Line = line; Column = 0; Procedure = ""; Module = moduleName }

    let compilePrograms (options: ProjectOptions) (programs: (ModuleSource * Ast.Program) list) =
        let modules =
            programs |> List.map (fun (m, program) ->
                ({ Name = m.Name; Kind = toCompilerKind m.Kind; Program = program } : Compiler.CompileModule))
        let hostNames = options.HostGlobals |> Map.toSeq |> Seq.map fst
        try Ok (Compiler.compileModules hostNames modules)
        with
        | Compiler.CompileFailure (message, line, moduleName) -> Error (compileFailure options message line moduleName)
        | ex -> Error (compileFailure options ex.Message 0 "")

module Project =
    /// Parse and compile modules together. Procedures are addressable as
    /// `Module.Proc`, and public procedures of standard modules also by bare
    /// name; Public module-level variables are shared, Private ones are
    /// per-module. Returns every syntax error, or the first compile error.
    let compile (options: ProjectOptions) (modules: ModuleSource list) : Result<Project, VbFailure list> =
        let parsed = modules |> List.map (fun m -> m, ProjectInternals.parseModule options m)
        let errors = parsed |> List.choose (fun (_, r) -> match r with Error e -> Some e | Ok _ -> None)
        if not errors.IsEmpty then Error errors
        else
            let programs = parsed |> List.map (fun (m, r) -> match r with Ok p -> m, p | Error _ -> failwith "unreachable")
            match ProjectInternals.compilePrograms options programs with
            | Error failure -> Error [ failure ]
            | Ok (bytecode, _) ->
                Ok { Options = options; Modules = modules; Bytecode = bytecode; Programs = programs
                     Procedures = programs |> List.collect (fun (m, p) -> ProjectInternals.procedures m p) }

    /// Compile a single source as an unnamed standard module.
    let ofSource (options: ProjectOptions) (source: string) =
        compile options [ { Name = ""; Kind = Standard; Source = source } ]

    let procedures (project: Project) = project.Procedures

    /// Public parameterless Subs of standard and document modules.
    let macros (project: Project) = project.Procedures |> List.filter (fun p -> p.IsMacro)

    /// Find a procedure by `Module.Name` or bare name (case-insensitive).
    let tryFindProcedure (name: string) (project: Project) =
        project.Procedures |> List.tryFind (fun p -> equalsIgnoreCase p.QualifiedName name)
        |> Option.orElse (project.Procedures |> List.tryFind (fun p -> equalsIgnoreCase p.Name name && p.ModuleKind = Standard))

/// A running instance of a project. Module-level state (variables, Err)
/// survives between calls, so the session backs both macro execution and an
/// Immediate window. One execution runs at a time; drive it with `Step`
/// (cooperative slices) or `RunToEnd`.
type Session private (project: Project, ctx: Compiler.CompileContext, state: StackVm.VmState) =
    let mutable running = false
    let mutable status = Finished VEmpty

    let reset () =
        state.Csp <- 1
        for i in 1 .. state.CallStack.Length - 1 do state.CallStack.[i] <- Unchecked.defaultof<StackVm.StackFrame>
        while state.Sp > 0 do
            state.Sp <- state.Sp - 1
            state.Stack.[state.Sp] <- VEmpty
        state.GoSubSp <- 0
        state.BaseCsp <- 1
        state.CancelRequested <- false
        state.Pc <- StackVm.hostReturnAddress
        let main = state.CallStack.[0]
        main.ErrorMode <- StackVm.NoHandler
        main.HandlerActive <- false
        running <- false

    let busy () =
        { Kind = Runtime; Number = 17; Description = "Can't perform requested operation: the session is already running"
          Source = project.Options.ProjectName; Line = 0; Column = 0; Procedure = ""; Module = "" }

    let refreshProgram () =
        let program = Compiler.toProgram ctx
        state.Program <- program
        state.Constants <- program.Constants
        if state.Globals.Length < max 1 program.Globals then
            let grown = Array.create program.Globals VEmpty
            System.Array.Copy(state.Globals, grown, state.Globals.Length)
            state.Globals <- grown

    let startExecution (entry: unit -> unit) =
        if running then Error (busy ())
        else
            entry ()
            running <- true
            status <- Running
            Ok ()

    /// Start a session: compile the project afresh for this session and
    /// queue its module-level code (run it with Step / RunToEnd).
    static member Start(project: Project) =
        match ProjectInternals.compilePrograms project.Options project.Programs with
        | Error failure -> failwith (VbFailure.format failure)
        | Ok (bytecode, ctx) ->
            let state = StackVm.createState project.Options.Hooks (project.Options.HostGlobals |> Map.toSeq) bytecode
            state.ProjectName <- project.Options.ProjectName
            let session = new Session(project, ctx, state)
            session.BeginInitialization()
            session

    /// Start a session and run its module-level code to completion.
    static member Create(project: Project, ?maxInstructions: int) =
        let session = Session.Start project
        match session.RunToEnd(?maxInstructions = maxInstructions) with
        | Failed failure -> Error failure
        | _ -> Ok session

    member private _.BeginInitialization() =
        running <- true
        status <- Running
        state.Pc <- 0

    member _.Project = project
    /// The VM state, for advanced inspection.
    member _.State = state
    member _.IsRunning = running
    /// The last status `Step` returned.
    member _.Status = status

    /// The current line / procedure while an execution is paused between slices.
    member _.Location =
        if not running then None
        else
            let line, procedure, moduleName = StackVm.currentLocation state
            Some { Line = line; Procedure = procedure; Module = moduleName }

    /// Run at most `maxInstructions` instructions of the current execution.
    member _.Step(maxInstructions: int) : ExecutionStatus =
        if not running then status
        else
            let result =
                try
                    if StackVm.runSlice state (max 1 maxInstructions) then
                        let value = if state.Csp = 1 && state.Sp > 0 then ValueSemantics.dereference state.Stack.[state.Sp - 1] else VEmpty
                        reset ()
                        Finished value
                    else Running
                with
                | :? StackVm.VbRuntimeException as e ->
                    reset ()
                    Failed (VbFailure.ofRuntime e.Failure)
                | ex ->
                    let line, procedure, moduleName = StackVm.currentLocation state
                    reset ()
                    Failed { Kind = Runtime; Number = 51; Description = ex.Message; Source = project.Options.ProjectName
                             Line = line; Column = 0; Procedure = procedure; Module = moduleName }
            status <- result
            result

    /// Run the current execution to its end (or until `maxInstructions` more
    /// instructions have run, returning `Running`).
    member this.RunToEnd(?maxInstructions: int) : ExecutionStatus =
        let limit = defaultArg maxInstructions System.Int32.MaxValue
        let mutable remaining = limit
        let mutable result = this.Step(min remaining 100000)
        remaining <- remaining - 100000
        while result = Running && remaining > 0 do
            result <- this.Step(min remaining 100000)
            remaining <- remaining - 100000
        result

    /// Ask the current execution to stop; the next Step fails with error 18.
    member _.Cancel() =
        if running then state.CancelRequested <- true

    /// Begin calling a procedure by `Module.Proc` or bare name. Omitted
    /// Optional arguments take their defaults (or Missing).
    member _.Call(name: string, args: Value list) : Result<unit, VbFailure> =
        let notFound () =
            Error { Kind = Runtime; Number = 35; Description = errorDescription 35 + ": " + name
                    Source = project.Options.ProjectName; Line = 0; Column = 0; Procedure = ""; Module = "" }
        let info = Project.tryFindProcedure name project
        let key = name.ToLowerInvariant()
        let found =
            match ctx.FunctionIndex.TryGetValue key with
            | true, index -> Some index
            | _ ->
                match info with
                | Some p ->
                    match ctx.FunctionIndex.TryGetValue(p.QualifiedName.ToLowerInvariant()) with
                    | true, index -> Some index
                    | _ -> None
                | None -> None
        match found, info with
        | Some index, _ when running -> ignore index; Error (busy ())
        | Some index, _ ->
            let funcDef = state.Program.Functions.[index]
            let parameters = info |> Option.map (fun p -> p.Parameters) |> Option.defaultValue []
            let args = List.toArray args
            if args.Length > funcDef.ParamsCount then
                Error { Kind = Runtime; Number = 450; Description = errorDescription 450; Source = project.Options.ProjectName
                        Line = 0; Column = 0; Procedure = name; Module = funcDef.Module }
            else
                startExecution (fun () ->
                    StackVm.pushFrameFor state StackVm.hostReturnAddress funcDef.LocalsCount index
                    let frame = state.CallStack.[state.Csp - 1]
                    for i in 0 .. funcDef.ParamsCount - 1 do
                        frame.Locals.[i] <-
                            if i < args.Length && args.[i] <> VUndefined then args.[i]
                            else
                                match List.tryItem i parameters with
                                | Some p when p.IsParamArray -> VArray { Data = [||]; LowerBounds = [| 0 |]; Lengths = [| 0 |] }
                                | Some p ->
                                    match p.DefaultValue, p.TypeName |> Option.bind ValueSemantics.declaredTypeOf with
                                    | Some v, _ -> v
                                    | None, Some dt -> ValueSemantics.defaultOf dt
                                    | None, None -> VUndefined
                                | None -> VUndefined
                    state.BaseCsp <- state.Csp
                    state.Pc <- funcDef.StartPC)
        | None, _ -> notFound ()

    member private _.BeginSnippet(moduleName: string, body: Choice<Expression, Statement list>) =
        if running then Error (busy ())
        else
            match (try Ok (Compiler.compileSnippet ctx moduleName body)
                   with
                   | Compiler.CompileFailure (message, line, _) -> Error (ProjectInternals.compileFailure project.Options message line moduleName)
                   | ex -> Error (ProjectInternals.compileFailure project.Options ex.Message 0 moduleName)) with
            | Error failure -> Error failure
            | Ok (start, locals) ->
                refreshProgram ()
                startExecution (fun () ->
                    StackVm.pushFrameFor state StackVm.hostReturnAddress locals -1
                    state.BaseCsp <- state.Csp
                    state.Pc <- start)

    /// Begin evaluating an expression in the context of `moduleName`.
    member this.Evaluate(expression: string, ?moduleName: string) : Result<unit, VbFailure> =
        match ParserSimple.parseExpression expression with
        | Error (message, line, column) ->
            Error { Kind = Syntax; Number = 0; Description = message; Source = project.Options.ProjectName
                    Line = line; Column = column; Procedure = ""; Module = "" }
        | Ok expr -> this.BeginSnippet(defaultArg moduleName "", Choice1Of2 expr)

    /// Begin executing statements (separated by newlines or ':'). Undeclared
    /// variables become session variables that later snippets can read.
    member this.Execute(statements: string, ?moduleName: string) : Result<unit, VbFailure> =
        match ParserSimple.parseDetailed project.Options.Dialect statements with
        | Error (message, line, column) ->
            Error { Kind = Syntax; Number = 0; Description = message; Source = project.Options.ProjectName
                    Line = line; Column = column; Procedure = ""; Module = "" }
        | Ok program ->
            let stmts = program.TopLevels |> List.choose (function TopLevelStatement s -> Some s | _ -> None)
            if stmts.Length <> program.TopLevels.Length then
                Error { Kind = Compile; Number = 0; Description = "Invalid in Immediate pane"; Source = project.Options.ProjectName
                        Line = 0; Column = 0; Procedure = ""; Module = "" }
            else this.BeginSnippet(defaultArg moduleName "", Choice2Of2 stmts)

    /// Begin an Immediate-window line: `?expr` or `Print expr` evaluates,
    /// anything else executes as statements.
    member this.Immediate(line: string, ?moduleName: string) : Result<unit, VbFailure> =
        let text = line.Trim()
        let moduleName = defaultArg moduleName ""
        if text.StartsWith "?" then this.Evaluate(text.Substring 1, moduleName)
        elif text.Length > 6 && equalsIgnoreCase (text.Substring(0, 6)) "print " then this.Evaluate(text.Substring 6, moduleName)
        else this.Execute(text, moduleName)

    /// Call a procedure and run it to the end.
    member this.Run(name: string, args: Value list, ?maxInstructions: int) : ExecutionStatus =
        match this.Call(name, args) with
        | Error failure -> Failed failure
        | Ok () -> this.RunToEnd(?maxInstructions = maxInstructions)

    /// Run an Immediate-window line to the end.
    member this.RunImmediate(line: string, ?moduleName: string, ?maxInstructions: int) : ExecutionStatus =
        match this.Immediate(line, ?moduleName = moduleName) with
        | Error failure -> Failed failure
        | Ok () -> this.RunToEnd(?maxInstructions = maxInstructions)

    /// A module-level or session variable (`Module.Name` for Private ones).
    member _.TryGetGlobal(name: string) = tryGetGlobal name state

    member _.SetGlobal(name: string, value: Value) =
        match state.Program.GlobalNames |> Array.tryFindIndex (fun n -> equalsIgnoreCase n name) with
        | Some index when index < state.Globals.Length -> state.Globals.[index] <- value; true
        | _ -> false

    /// Replace the hooks (e.g. per run).
    member _.SetHooks(hooks: RuntimeHooks) = state.Hooks <- hooks
