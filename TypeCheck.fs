/// Early binding, as VBA's compiler does it (checked against Microsoft Excel 2019: rbt-office's
/// tests/vba-parity/compile-cases): a variable declared with an object type (`Dim r As Range`, a parameter
/// `ws As Worksheet`, a function `As Range`) has the members its type library gives that type. A member of
/// a non-extensible interface (Excel's Worksheet, Word's Range; a VBA class) that it does not have is a
/// compile error, "Method or data member not found"; an extensible one (Excel's Range, Application) may
/// have members its library does not list, found when they run. A known member is checked either way: its
/// arguments, and writing it when it cannot be written. Expressions typed Object or Variant, and types no
/// library describes, are bound late.
///
/// The libraries are the host's (Excel's, Word's), as TSV made from their type libraries:
///     member <TAB> interface <TAB> name <TAB> method|get|let|set <TAB> parameters (name[?];...) <TAB> returns
///     coclass <TAB> name <TAB> default interface        alias <TAB> name <TAB> type
///     enum <TAB> enum <TAB> name <TAB> value          interface <TAB> name <TAB> extensible|nonextensible
/// VBA classes of the project are types too: their public members.
module Rbt.Vb.TypeCheck

open System
open System.Collections.Generic
open Rbt.Vb.Ast
open Rbt.Vb.Values

type MemberKind = MethodMember | GetMember | LetMember | SetMember

/// A member of an interface: its kind, its parameters (name, optional) and the type it returns ("" none).
type MemberSignature = { Name: string; Kind: MemberKind; Parameters: (string * bool) list; Returns: string }

/// What a library describes: interfaces and their members, classes (Worksheet is _Worksheet), enums.
type Library = {
    /// By lower-cased interface name: its members by lower-cased name (a property has get, let, set).
    Interfaces: Dictionary<string, Dictionary<string, MemberSignature list>>
    /// Lower-cased class or alias name to the type it stands for.
    Classes: Dictionary<string, string>
    /// Lower-cased enum names (a variable of an enum type is a Long).
    Enums: HashSet<string>
    /// Lower-cased interface name to the name the library gives it (for messages).
    Names: Dictionary<string, string>
    /// Lower-cased names of the interfaces whose members VBA checks when it compiles.
    NonExtensible: HashSet<string>
    /// The enums' members: constants, as the library names them.
    Constants: (string * Value) list
}

module Library =
    let empty () =
        { Interfaces = Dictionary(); Classes = Dictionary(); Enums = HashSet(); Names = Dictionary(); NonExtensible = HashSet(); Constants = [] }

    /// A library from its TSV (several are merged with `merge`).
    let parse (text: string) : Library =
        let library = empty ()
        let constants = ResizeArray<string * Value>()
        for line in text.Split('\n') do
            let fields = line.TrimEnd('\r').Split('\t')
            match fields with
            | [| "member"; iface; name; kind; parameters; returns |] ->
                let key = iface.ToLowerInvariant()
                library.Names.[key] <- iface
                let members =
                    match library.Interfaces.TryGetValue key with
                    | true, found -> found
                    | _ ->
                        let created = Dictionary<string, MemberSignature list>()
                        library.Interfaces.[key] <- created
                        created
                let kind = match kind with "get" -> GetMember | "let" -> LetMember | "set" -> SetMember | _ -> MethodMember
                let ps =
                    if parameters = "" then []
                    else parameters.Split(';') |> Array.toList |> List.map (fun p -> if p.EndsWith "?" then p.Substring(0, p.Length - 1), true else p, false)
                let signature = { Name = name; Kind = kind; Parameters = ps; Returns = returns }
                let memberKey = name.ToLowerInvariant()
                members.[memberKey] <- (match members.TryGetValue memberKey with | true, known -> known @ [ signature ] | _ -> [ signature ])
            | [| "coclass"; name; iface |] | [| "alias"; name; iface |] -> library.Classes.[name.ToLowerInvariant()] <- iface
            | [| "interface"; name; "nonextensible" |] -> library.NonExtensible.Add(name.ToLowerInvariant()) |> ignore
            | [| "enum"; enumName; name; value |] ->
                library.Enums.Add(enumName.ToLowerInvariant()) |> ignore
                match Int64.TryParse value with
                | true, n when n >= int64 Int32.MinValue && n <= int64 Int32.MaxValue -> constants.Add((name, VLong (int n)))
                | true, n -> constants.Add((name, VDouble (float n)))
                | _ -> constants.Add((name, VString value))
            | _ -> ()
        { library with Constants = List.ofSeq constants }

    let merge (libraries: Library list) : Library =
        let merged = empty ()
        for library in libraries do
            for KeyValue (name, members) in library.Interfaces do
                if not (merged.Interfaces.ContainsKey name) then merged.Interfaces.[name] <- members
            for KeyValue (name, target) in library.Classes do
                if not (merged.Classes.ContainsKey name) then merged.Classes.[name] <- target
            for name in library.Enums do merged.Enums.Add name |> ignore
            for KeyValue (key, name) in library.Names do merged.Names.[key] <- name
            for name in library.NonExtensible do merged.NonExtensible.Add name |> ignore
        { merged with Constants = libraries |> List.collect (fun l -> l.Constants) }

/// The types a host gives its project: its libraries, the interface whose members are globals (Excel's and
/// Word's _Global: Range(...), Cells, ActiveDocument), and the types of its other globals (Sheet1 is a
/// Worksheet, ThisWorkbook a Workbook) and of the document modules' Me.
type HostTypes = {
    Library: Library
    GlobalInterface: string option
    Globals: Map<string, string>
}

/// A compile error the checker found.
type Problem = { Module: string; Procedure: string; Line: int; Description: string; Detail: string }

// ---- types of expressions ------------------------------------------------------------------------------

type private Ty =
    | Interface of string          // a library interface (lower-cased)
    | VbClass of string            // a class module of the project (lower-cased)
    | Late                         // Object, Variant, a value, a type no library describes

type private ClassInfo = { Name: string; Members: Dictionary<string, string option> }   // member -> its type name

type private Variable = { Type: string; IsArray: bool }

let private lower (s: string) = s.ToLowerInvariant()

/// `Excel.Range` is `Range`.
let private unqualified (name: string) = match name.LastIndexOf '.' with -1 -> name | at -> name.Substring(at + 1)

type private Checker(host: HostTypes, classes: Dictionary<string, ClassInfo>, problems: ResizeArray<Problem>) =
    let library = host.Library

    member this.Resolve(typeName: string) : Ty =
        let rec go (name: string) (depth: int) =
            let key = lower (unqualified name)
            if key = "" || key = "object" || key = "variant" || depth > 8 then Late
            elif classes.ContainsKey key then VbClass key
            elif library.Interfaces.ContainsKey key then Interface key
            else
                match library.Classes.TryGetValue key with
                | true, target -> go target (depth + 1)
                | _ -> Late
        go typeName 0

    /// The signatures of a member of an interface (None: the interface has no such member).
    member _.Signatures(iface: string, name: string) : MemberSignature list option =
        match library.Interfaces.TryGetValue iface with
        | true, members ->
            match members.TryGetValue (lower name) with
            | true, found -> Some found
            | _ -> None
        | _ -> None

    member this.ReadType(signatures: MemberSignature list) : Ty =
        match signatures |> List.tryFind (fun s -> s.Kind = GetMember || s.Kind = MethodMember) with
        | Some s -> this.Resolve s.Returns
        | None -> Late

    member _.Report(scope: Scope, line: int, description: string, detail: string) =
        if not (problems |> Seq.exists (fun p -> p.Module = scope.Module && p.Line = line && p.Detail = detail)) then
            problems.Add { Module = scope.Module; Procedure = scope.Procedure; Line = line; Description = description; Detail = detail }

    /// The type of `target.name` (reported when `target` is of a type without that member).
    member this.MemberType(scope: Scope, line: int, target: Ty, name: string, typeLabel: string) : Ty * MemberSignature list option =
        match target with
        | Interface iface ->
            match this.Signatures(iface, name) with
            | Some signatures -> this.ReadType signatures, Some signatures
            | None ->
                if library.NonExtensible.Contains iface then
                    this.Report(scope, line, "Method or data member not found", typeLabel + "." + name)
                Late, None
        | VbClass cls ->
            match classes.[cls].Members.TryGetValue (lower name) with
            | true, Some t -> this.Resolve t, None
            | true, None -> Late, None
            | _ ->
                this.Report(scope, line, "Method or data member not found", classes.[cls].Name + "." + name)
                Late, None
        | Late -> Late, None

    member this.Label(ty: Ty) =
        match ty with
        | Interface iface -> (match library.Names.TryGetValue iface with | true, name -> name | _ -> iface)
        | VbClass cls -> classes.[cls].Name
        | Late -> ""

    /// The arguments of a call of a member: more than it has parameters, or a name it has not, is a compile
    /// error - unless what it returns is an object, whose default member may take them (`r.Cells(1, 1)`).
    member this.CheckArguments(scope: Scope, line: int, signatures: MemberSignature list, args: Expression list, about: string) =
        match signatures |> List.tryFind (fun s -> s.Kind = MethodMember || s.Kind = GetMember) with
        // (The arguments are the member's when it has parameters or returns no object; a parameterless
        // member that returns an object passes them to its default member: `r.Cells(1, 1)`.)
        | Some s when not s.Parameters.IsEmpty || (match this.Resolve s.Returns with Late -> true | _ -> false) ->
            let named = args |> List.choose (function NamedArg (n, _) -> Some n | _ -> None)
            let positional = args |> List.filter (function NamedArg _ -> false | _ -> true) |> List.length
            // (A library's last parameter named ParamArray-like "Arg30" lists them all; none takes more.)
            if positional > s.Parameters.Length then
                this.Report(scope, line, "Wrong number of arguments or invalid property assignment", about)
            for n in named do
                if not (s.Parameters |> List.exists (fun (p, _) -> lower p = lower n)) then
                    this.Report(scope, line, "Named argument not found", about + " " + n)
        | _ -> ()

    /// The type of an expression, checking the members it reaches.
    member this.TypeOf(scope: Scope, line: int, e: Expression) : Ty =
        match e with
        | Identifier name -> this.NameType(scope, name, false)
        | MeExpr -> scope.Me
        | WithDotExpr -> (match scope.With with t :: _ -> t | [] -> Late)
        | NewExpr cls -> this.Resolve cls
        | Parenthesized inner -> this.TypeOf(scope, line, inner)
        | Member (target, name) ->
            let t = this.TypeOf(scope, line, target)
            fst (this.MemberType(scope, line, t, name, this.Label t))
        | Call (Member (target, name), args) ->
            let t = this.TypeOf(scope, line, target)
            for a in args do this.Check(scope, line, a)
            match this.MemberType(scope, line, t, name, this.Label t) with
            | result, Some signatures -> this.CheckArguments(scope, line, signatures, args, this.Label t + "." + name); result
            | result, None -> result
        | Call (Identifier name, args) ->
            for a in args do this.Check(scope, line, a)
            this.NameType(scope, name, true)
        | Call (target, args) ->
            this.Check(scope, line, target)
            for a in args do this.Check(scope, line, a)
            Late
        | Binary (_, l, r) -> this.Check(scope, line, l); this.Check(scope, line, r); Late
        | Unary (_, x) | NamedArg (_, x) | TypeOfIs (x, _) -> this.Check(scope, line, x); Late
        | ArrayBound (a, b) -> this.Check(scope, line, a); this.Check(scope, line, b); Late
        | Expression.Literal _ | MissingArg -> Late

    member this.Check(scope: Scope, line: int, e: Expression) = this.TypeOf(scope, line, e) |> ignore

    /// The type of a name: a variable (an array's element when indexed), a function's result, a global.
    member this.NameType(scope: Scope, name: string, indexed: bool) : Ty =
        let key = lower name
        let variable =
            match scope.Locals.TryGetValue key with
            | true, v -> Some v
            | _ ->
                match scope.ModuleVariables.TryGetValue key with
                | true, v -> Some v
                | _ -> match scope.ProjectVariables.TryGetValue key with | true, v -> Some v | _ -> None
        match variable with
        | Some v -> if v.IsArray && not indexed then Late else this.Resolve v.Type
        | None ->
            match scope.Functions.TryGetValue key with
            | true, returns -> (match returns with Some t -> this.Resolve t | None -> Late)
            | _ ->
                match host.Globals |> Map.tryFind key with
                | Some t -> this.Resolve t
                | None ->
                    match host.GlobalInterface with
                    | Some iface ->
                        match this.Signatures(lower iface, name) with
                        | Some signatures -> this.ReadType signatures
                        | None -> Late
                    | None -> Late

and private Scope = {
    Module: string
    Procedure: string
    Locals: Dictionary<string, Variable>
    ModuleVariables: Dictionary<string, Variable>
    ProjectVariables: Dictionary<string, Variable>
    Functions: Dictionary<string, string option>
    Me: Ty
    With: Ty list
}

let private declared (d: VarDeclarator) =
    d.Type |> Option.map (fun t -> { Type = t.Name; IsArray = d.ArraySpec.IsSome })

let private isPublic (mods: Modifier list) =
    not (mods |> List.exists (function VisibilityMod Private -> true | _ -> false))

let private functionParts (f: Function) =
    match f with
    | FunctionDecl (mods, name, ps, returns, body) -> mods, name, ps, returns, body
    | SubDecl (mods, name, ps, body) -> mods, name, ps, None, body
    | PropertyGet (mods, name, ps, returns, body) -> mods, name, ps, returns, body
    | PropertyLet (mods, name, ps, body) | PropertySet (mods, name, ps, body) -> mods, name, ps, None, body

let rec private statements (s: Statement) : Statement list =
    match s with
    | StatementGroup items -> items |> List.collect statements
    | other -> [ other ]

/// What a module of the project is: its kind and parsed program, and its name.
type CheckedModule = { Name: string; IsClass: bool; MeType: string option; Program: Program }

/// The compile errors of early-bound members in a project (empty when there are none).
let check (host: HostTypes) (modules: CheckedModule list) : Problem list =
    let problems = ResizeArray<Problem>()
    let tops (m: CheckedModule) = m.Program.TopLevels
    // ---- the project's classes and public names ----
    let classes = Dictionary<string, ClassInfo>()
    for m in modules do
        if m.IsClass then
            let members = Dictionary<string, string option>()
            for t in tops m do
                match t with
                | FunctionDef f ->
                    let mods, name, _, returns, _ = functionParts f
                    if isPublic mods then members.[lower name] <- returns |> Option.map (fun r -> r.Name)
                | TopLevelStatement stmt ->
                    for s in statements (match stmt with LocatedStmt (_, x) -> x | x -> x) do
                        match s with
                        | Declaration (vis, Dim ds) when (match vis with Some Private -> false | Some _ -> true | None -> false) ->
                            for d in ds do members.[lower d.Name] <- d.Type |> Option.map (fun t -> t.Name)
                        | _ -> ()
                | WithEventsDecl (vis, name, t) when vis <> Some Private -> members.[lower name] <- Some t.Name
                | _ -> ()
            classes.[lower m.Name] <- { Name = m.Name; Members = members }
    let checker = Checker(host, classes, problems)
    let projectVariables = Dictionary<string, Variable>()
    let projectFunctions = Dictionary<string, string option>()
    for m in modules do
        if not m.IsClass then
            for t in tops m do
                match t with
                | FunctionDef f ->
                    let mods, name, _, returns, _ = functionParts f
                    if isPublic mods then projectFunctions.[lower name] <- returns |> Option.map (fun r -> r.Name)
                | TopLevelStatement stmt ->
                    for s in statements (match stmt with LocatedStmt (_, x) -> x | x -> x) do
                        match s with
                        | Declaration (Some (Public | Global), Dim ds) ->
                            for d in ds do declared d |> Option.iter (fun v -> projectVariables.[lower d.Name] <- v)
                        | _ -> ()
                | _ -> ()
    // ---- each procedure ----
    for m in modules do
        let moduleVariables = Dictionary<string, Variable>()
        let functions = Dictionary<string, string option>(projectFunctions)
        for t in tops m do
            match t with
            | FunctionDef f ->
                let _, name, _, returns, _ = functionParts f
                functions.[lower name] <- returns |> Option.map (fun r -> r.Name)
            | TopLevelStatement stmt ->
                for s in statements (match stmt with LocatedStmt (_, x) -> x | x -> x) do
                    match s with
                    | Declaration (_, Dim ds) | Declaration (_, StaticDim ds) ->
                        for d in ds do declared d |> Option.iter (fun v -> moduleVariables.[lower d.Name] <- v)
                    | _ -> ()
            | WithEventsDecl (_, name, t) -> moduleVariables.[lower name] <- { Type = t.Name; IsArray = false }
            | _ -> ()
        // (Me in a class reaches its private members too: not checked.)
        let me = if m.IsClass then Late else match m.MeType with Some t -> checker.Resolve t | None -> Late
        for t in tops m do
            match t with
            | FunctionDef f ->
                let _, name, ps, _, body = functionParts f
                let locals = Dictionary<string, Variable>()
                for p in ps do
                    match p.Type with
                    | Some t -> locals.[lower p.Name] <- { Type = t.Name; IsArray = p.IsArray }
                    | None -> locals.[lower p.Name] <- { Type = "Variant"; IsArray = p.IsArray }
                // (Declarations anywhere in the procedure hold for all of it, as VBA's.)
                let rec collect (s: Statement) =
                    match s with
                    | LocatedStmt (_, x) -> collect x
                    | StatementGroup xs -> List.iter collect xs
                    | Declaration (_, Dim ds) | Declaration (_, StaticDim ds) ->
                        for d in ds do locals.[lower d.Name] <- (declared d |> Option.defaultValue { Type = "Variant"; IsArray = d.ArraySpec.IsSome })
                    | IfStmt (_, a, elseIfs, b) ->
                        List.iter collect a
                        for _, x in elseIfs do List.iter collect x
                        b |> Option.iter (List.iter collect)
                    | SelectCase (_, cases) -> for _, x in cases do List.iter collect x
                    | ForLoop (_, _, _, _, x) | ForEach (_, _, x) | WhileLoop (_, x) | WithStmt (_, x) -> List.iter collect x
                    | DoLoop (a, b) ->
                        a |> Option.iter (fun c -> List.iter collect c.Body)
                        b |> Option.iter (fun c -> List.iter collect c.Body)
                    | _ -> ()
                List.iter collect body
                let scope =
                    { Module = m.Name; Procedure = name; Locals = locals; ModuleVariables = moduleVariables
                      ProjectVariables = projectVariables; Functions = functions; Me = me; With = [] }
                let rec walk (scope: Scope) (line: int) (s: Statement) =
                    let check e = checker.Check(scope, line, e)
                    match s with
                    | LocatedStmt (l, x) -> walk scope l x
                    | StatementGroup xs -> List.iter (walk scope line) xs
                    | ExpressionStmt e -> check e
                    | Statement.Let (_, e) | Statement.Set (_, e) | Assignment (_, e) -> check e
                    | CallStmt (target, args) -> check (Call (target, args))
                    | IndexedAssignment (_, idx, v, _) -> List.iter check idx; check v
                    | MemberAssignment (target, name, value, isSet) ->
                        check value
                        let t = checker.TypeOf(scope, line, target)
                        match checker.MemberType(scope, line, t, name, checker.Label t) with
                        | result, Some signatures ->
                            // (A property without Let / Set: assigned only through what it reads, an object's
                            // default member - `ws.UsedRange = 1`.)
                            let writable = signatures |> List.exists (fun s -> s.Kind = (if isSet then SetMember else LetMember))
                            let viaDefault = not isSet && (match result with Late -> false | _ -> true)
                            if not writable && not viaDefault then
                                let message = if isSet then "Invalid use of property" else "Wrong number of arguments or invalid property assignment"
                                checker.Report(scope, line, message, checker.Label t + "." + name)
                        | _ -> ()
                    | DefaultAssignment (target, value, _) -> check target; check value
                    | IfStmt (c, a, elseIfs, b) ->
                        check c
                        List.iter (walk scope line) a
                        for c2, x in elseIfs do check c2; List.iter (walk scope line) x
                        b |> Option.iter (List.iter (walk scope line))
                    | SelectCase (e, cases) ->
                        check e
                        for tests, x in cases do
                            tests |> Option.iter (List.iter (function
                                | CaseValue v -> check v
                                | CaseRange (a, b) -> check a; check b
                                | CaseComparison (_, v) -> check v))
                            List.iter (walk scope line) x
                    | ForLoop (_, a, b, c, x) -> check a; check b; Option.iter check c; List.iter (walk scope line) x
                    | ForEach (_, e, x) -> check e; List.iter (walk scope line) x
                    | WhileLoop (c, x) -> check c; List.iter (walk scope line) x
                    | DoLoop (a, b) ->
                        for d in [ a; b ] do
                            d |> Option.iter (fun c -> check c.Condition; List.iter (walk scope line) c.Body)
                    | WithStmt (e, x) ->
                        let t = checker.TypeOf(scope, line, e)
                        List.iter (walk { scope with With = t :: scope.With } line) x
                    | RaiseEventStmt (_, args) -> List.iter check args
                    | Declaration (_, ReDim (_, dims, _)) -> List.iter check dims
                    | _ -> ()
                List.iter (walk scope 0) body
            | _ -> ()
    List.ofSeq problems
