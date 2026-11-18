module Rbt.Vb.Compiler

open Ast
open Bytecode
open Values
open System.Collections.Generic

let private normalizeName (name: string) =
#if FABLE_COMPILER
    if isNull (box name) then "" else name.ToLowerInvariant()
#else
    name.ToLowerInvariant()
#endif

let private newNameDictionary<'T> () =
#if FABLE_COMPILER
    Dictionary<string, 'T>()
#else
    Dictionary<string, 'T>(System.StringComparer.OrdinalIgnoreCase)
#endif

let private newNameSet () =
#if FABLE_COMPILER
    HashSet<string>()
#else
    HashSet<string>(System.StringComparer.OrdinalIgnoreCase)
#endif

let private containsName (dictionary: Dictionary<string, 'T>) name =
    dictionary.ContainsKey(normalizeName name)

let private tryGetName (dictionary: Dictionary<string, 'T>) name =
    dictionary.TryGetValue(normalizeName name)

let private getName (dictionary: Dictionary<string, 'T>) name =
    dictionary.[normalizeName name]

let private setName (dictionary: Dictionary<string, 'T>) name value =
    dictionary.[normalizeName name] <- value

let private containsSetName (set: HashSet<string>) name =
    set.Contains(normalizeName name)

let private addSetName (set: HashSet<string>) name =
    set.Add(normalizeName name)

let private copyNameDictionary (source: Dictionary<string, 'T>) =
    let copy = newNameDictionary ()
    for kv in source do
        setName copy kv.Key kv.Value
    copy

/// A compile error with the 1-based source line it was found on (0 if unknown).
exception CompileFailure of message: string * line: int * moduleName: string

/// How a module is compiled.
type ModuleKind =
    | StandardModule
    | ClassModule
    | DocumentModule

type CompileContext = {
    Instructions: ResizeArray<Instruction>
    Constants: ResizeArray<Value>
    ConstantIndex: Dictionary<Value, int>
    Globals: Dictionary<string, int>
    mutable Locals: Dictionary<string, int>
    Functions: ResizeArray<FunctionDef>
    FunctionIndex: Dictionary<string, int>
    Classes: ResizeArray<ClassDef>
    ClassIndex: Dictionary<string, int>
    ArrayVars: HashSet<string>
    mutable ClassFields: HashSet<string> option
    mutable ClassName: string option
    ExitLoopPatches: ResizeArray<ResizeArray<int>>
    Labels: Dictionary<string, int>
    LabelPatches: ResizeArray<string * int>
    mutable NextGlobalSlot: int
    mutable NextLocalSlot: int
    mutable ReturnSlotName: string option
    mutable InFunction: bool
    mutable WithTargetSlot: int option
    /// Function key -> declared parameters (for ByRef, Optional and ParamArray).
    Parameters: Dictionary<string, Parameter list>
    /// Declared types of the current procedure's locals.
    mutable LocalTypes: Dictionary<string, string>
    /// Declared types of globals, keyed by global name.
    GlobalTypes: Dictionary<string, string>
    /// Enum name -> member names.
    Enums: Dictionary<string, HashSet<string>>
    /// Standard/document module names, for `Module.Member` qualification.
    ModuleNames: HashSet<string>
    mutable ModuleName: string
    ModuleStarts: ResizeArray<int * string>
    mutable CurrentLine: int
    mutable TextCompare: bool
    /// `Option Base 1` in the current module.
    mutable OptionBase: int
    /// `Option Explicit` in the current module: undeclared names are errors.
    mutable OptionExplicit: bool
    /// A ReDim may declare its array implicitly.
    mutable ImplicitAllowed: bool
    /// `Dim x As New Class`: variable key -> class name (globals and fields).
    AsNew: Dictionary<string, string>
    mutable LocalAsNew: Dictionary<string, string>
    /// Arrays declared with fixed bounds (`Dim a(10)`), for Erase.
    FixedArrays: HashSet<string>
    /// Static locals of the current procedure: name -> global key.
    mutable StaticLocals: Dictionary<string, string>
    /// Static locals to initialise when the program starts: module, key, declarator.
    StaticInits: ResizeArray<string * string * VarDeclarator>
    /// Some class has a Class_Terminate.
    mutable HasTerminate: bool
    /// Class name -> declared event names.
    ClassEvents: Dictionary<string, string list>
    /// "Scope.variable" -> event binding index.
    WithEventsVars: Dictionary<string, int>
    EventBindings: ResizeArray<EventBinding>
    /// Class name -> default member (from `Attribute X.VB_UserMemId = 0`).
    DefaultMembers: Dictionary<string, string>
    /// Function index -> declared return type name (`Function F() As Long`).
    FunctionTypes: Dictionary<int, string>
    /// Numeric line label -> the Erl it reports, for the current procedure.
    mutable ErlLines: Dictionary<string, int>
}

let emptyContext () =
    {
        Instructions = ResizeArray()
        Constants = ResizeArray()
        ConstantIndex = Dictionary<Value, int>()
        Globals = newNameDictionary ()
        Locals = newNameDictionary ()
        Functions = ResizeArray()
        FunctionIndex = newNameDictionary ()
        Classes = ResizeArray()
        ClassIndex = newNameDictionary ()
        ArrayVars = newNameSet ()
        ClassFields = None
        ClassName = None
        ExitLoopPatches = ResizeArray()
        Labels = newNameDictionary ()
        LabelPatches = ResizeArray()
        NextGlobalSlot = 0
        NextLocalSlot = 0
        ReturnSlotName = None
        InFunction = false
        WithTargetSlot = None
        Parameters = newNameDictionary ()
        LocalTypes = newNameDictionary ()
        GlobalTypes = newNameDictionary ()
        Enums = newNameDictionary ()
        ModuleNames = newNameSet ()
        ModuleName = ""
        ModuleStarts = ResizeArray()
        CurrentLine = 0
        TextCompare = false
        OptionBase = 0
        OptionExplicit = false
        ImplicitAllowed = false
        AsNew = newNameDictionary ()
        LocalAsNew = newNameDictionary ()
        FixedArrays = newNameSet ()
        StaticLocals = newNameDictionary ()
        StaticInits = ResizeArray()
        HasTerminate = false
        ClassEvents = newNameDictionary ()
        WithEventsVars = newNameDictionary ()
        EventBindings = ResizeArray()
        DefaultMembers = newNameDictionary ()
        FunctionTypes = Dictionary()
        ErlLines = Dictionary()
    }

let private fail (ctx: CompileContext) message : 'a =
    raise (CompileFailure (message, ctx.CurrentLine, ctx.ModuleName))

let emit (ctx: CompileContext) (opcode: Opcode) (line: int) =
    ctx.Instructions.Add({ Opcode = opcode; LineNumber = (if line = 0 then ctx.CurrentLine else line) })

let patchJump (ctx: CompileContext) (pc: int) (opcode: Opcode) =
    ctx.Instructions.[pc] <- { ctx.Instructions.[pc] with Opcode = opcode }

let addConstant (ctx: CompileContext) (value: Value) =
    let mutable foundIndex = 0
    if ctx.ConstantIndex.TryGetValue(value, &foundIndex) then
        foundIndex
    else
        let index = ctx.Constants.Count
        ctx.Constants.Add(value)
        ctx.ConstantIndex.[value] <- index
        index

let private emitConst ctx value = emit ctx (LoadConst (addConstant ctx value)) 0

let getGlobalSlot (ctx: CompileContext) (name: string) =
    match tryGetName ctx.Globals name with
    | true, slot -> slot
    | false, _ ->
        setName ctx.Globals name ctx.NextGlobalSlot
        let slot = ctx.NextGlobalSlot
        ctx.NextGlobalSlot <- ctx.NextGlobalSlot + 1
        slot

let getLocalSlot (ctx: CompileContext) (name: string) =
    match tryGetName ctx.Locals name with
    | true, slot -> slot
    | false, _ ->
        setName ctx.Locals name ctx.NextLocalSlot
        let slot = ctx.NextLocalSlot
        ctx.NextLocalSlot <- ctx.NextLocalSlot + 1
        slot

let private freshLocal (ctx: CompileContext) =
    let slot = ctx.NextLocalSlot
    ctx.NextLocalSlot <- slot + 1
    slot

let private isField (ctx: CompileContext) name =
    not (containsName ctx.Locals name) &&
    (match ctx.ClassFields with Some fields -> containsSetName fields name | None -> false)

/// The global key of a module-private name.
let private privateKey (moduleName: string) (name: string) = moduleName + "." + name

/// A global visible from the current module: its private variables first.
let private tryResolveGlobal (ctx: CompileContext) name =
    let privateSlot =
        if ctx.ModuleName = "" then None
        else
            match tryGetName ctx.Globals (privateKey ctx.ModuleName name) with
            | true, slot -> Some slot
            | _ -> None
    match privateSlot with
    | Some _ -> privateSlot
    | None ->
        match tryGetName ctx.Globals name with
        | true, slot -> Some slot
        | _ -> None

/// A procedure visible from the current module: its own procedures first.
let private tryResolveFunction (ctx: CompileContext) name =
    let qualified =
        if ctx.ModuleName = "" then None
        else
            match tryGetName ctx.FunctionIndex (privateKey ctx.ModuleName name) with
            | true, index -> Some (privateKey ctx.ModuleName name, index)
            | _ -> None
    match qualified with
    | Some _ -> qualified
    | None ->
        match tryGetName ctx.FunctionIndex name with
        | true, index -> Some (name, index)
        | _ -> None

type private VarLoc =
    | FieldLoc
    | GlobalLoc of int
    | LocalLoc of int
    | ConstLoc of Value

let private isLocalVariable (ctx: CompileContext) name =
    ctx.InFunction && containsName ctx.Locals name

let private staticKey (ctx: CompileContext) name =
    if ctx.InFunction then
        match tryGetName ctx.StaticLocals name with
        | true, key -> Some key
        | _ -> None
    else None

let private resolveVar (ctx: CompileContext) (name: string) (forWrite: bool) =
    match staticKey ctx name with
    | Some key -> GlobalLoc (getName ctx.Globals key)
    | None ->
    if isField ctx name then FieldLoc
    elif isLocalVariable ctx name then LocalLoc (getName ctx.Locals name)
    else
        match tryResolveGlobal ctx name with
        | Some slot -> GlobalLoc slot
        | None ->
            if containsName ctx.Locals name then LocalLoc (getName ctx.Locals name)
            else
                match (if forWrite then None else Builtins.tryConstant name) with
                | Some value -> ConstLoc value
                | None ->
                    if ctx.OptionExplicit && not ctx.ImplicitAllowed then
                        fail ctx (sprintf "Variable not defined: %s" name)
                    // Undeclared names in module-level code are module variables,
                    // so they survive into later host calls and snippets.
                    if not ctx.InFunction && ctx.ClassFields.IsNone then GlobalLoc (getGlobalSlot ctx name)
                    else LocalLoc (getLocalSlot ctx name)

/// The declared type name of a variable, if any.
let private declaredTypeName (ctx: CompileContext) name =
    match staticKey ctx name with
    | Some key -> (match tryGetName ctx.GlobalTypes key with | true, t -> Some t | _ -> None)
    | None ->
    if isField ctx name then None
    elif isLocalVariable ctx name || (containsName ctx.Locals name && (tryResolveGlobal ctx name).IsNone) then
        match tryGetName ctx.LocalTypes name with
        | true, t -> Some t
        | _ -> None
    else
        let key =
            if ctx.ModuleName <> "" && containsName ctx.Globals (privateKey ctx.ModuleName name) then privateKey ctx.ModuleName name
            else name
        match tryGetName ctx.GlobalTypes key with
        | true, t -> Some t
        | _ -> None

let private udtIndex (ctx: CompileContext) (typeName: string) =
    match tryGetName ctx.ClassIndex typeName with
    | true, idx when ctx.Classes.[idx].IsValueType -> Some idx
    | _ -> None

/// Emit the conversion for storing into a variable declared `As typeName`.
let private emitStoreConversion (ctx: CompileContext) (typeName: string option) =
    match typeName with
    | Some t ->
        match ValueSemantics.declaredTypeOf t with
        | Some dt -> emit ctx (Coerce (int dt)) 0
        | None -> if (udtIndex ctx t).IsSome then emit ctx CopyUdt 0
    | None -> ()

let private emitRawLoad (ctx: CompileContext) name =
    match resolveVar ctx name false with
    | FieldLoc ->
        emit ctx (LoadLocal 0) 0
        emit ctx (GetMember name) 0
    | GlobalLoc slot -> emit ctx (LoadGlobal slot) 0
    | LocalLoc slot -> emit ctx (LoadLocal slot) 0
    | ConstLoc value -> emitConst ctx value

/// The class of a `Dim x As New Class` variable.
let private asNewClass (ctx: CompileContext) name =
    let lookup (d: Dictionary<string, string>) key = match tryGetName d key with | true, c -> Some c | _ -> None
    match staticKey ctx name with
    | Some key -> lookup ctx.AsNew key
    | None ->
    if isField ctx name then
        match ctx.ClassName with
        | Some className -> lookup ctx.AsNew (className + "." + name)
        | None -> None
    elif isLocalVariable ctx name then lookup ctx.LocalAsNew name
    elif ctx.InFunction && containsName ctx.Locals name then None
    else
        if ctx.ModuleName <> "" && containsName ctx.Globals (privateKey ctx.ModuleName name) then lookup ctx.AsNew (privateKey ctx.ModuleName name)
        else lookup ctx.AsNew name

/// `New Class` for VB classes and the built-in Collection and Dictionary.
let private emitNew (ctx: CompileContext) (className: string) =
    match tryGetName ctx.ClassIndex className with
    | true, idx -> emit ctx (NewObj idx) 0
    | false, _ when Collections.isBuiltinClass className ->
        emitConst ctx (VString className)
        emit ctx (CallBuiltin ("new:builtin", 1)) 0
    | false, _ -> fail ctx (sprintf "User-defined type not defined: %s" className)

let rec private storeRawWith (ctx: CompileContext) name =
    match resolveVar ctx name true with
    | FieldLoc ->
        let temp = ctx.NextLocalSlot
        ctx.NextLocalSlot <- temp + 1
        emit ctx (StoreLocal temp) 0
        emit ctx (LoadLocal 0) 0
        emit ctx (LoadLocal temp) 0
        emit ctx (SetMember (name, false)) 0
    | GlobalLoc slot -> emit ctx (StoreGlobal slot) 0
    | LocalLoc slot -> emit ctx (StoreLocal slot) 0
    | ConstLoc _ -> ()

/// Create a `Dim x As New Class` object on first use.
let private emitEnsureNew (ctx: CompileContext) name =
    match asNewClass ctx name with
    | Some className ->
        emitRawLoad ctx name
        emitConst ctx VNothing
        emit ctx IsOp 0
        let skip = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        emitNew ctx className
        storeRawWith ctx name
        patchJump ctx skip (JumpIfFalse ctx.Instructions.Count)
    | None -> ()

let private emitLoadVar (ctx: CompileContext) name =
    emitEnsureNew ctx name
    emitRawLoad ctx name

/// Store the value on top of the stack into a variable.
let private storeName (ctx: CompileContext) name =
    emitStoreConversion ctx (declaredTypeName ctx name)
    match resolveVar ctx name true with
    | FieldLoc ->
        let temp = freshLocal ctx
        emit ctx (StoreLocal temp) 0
        emit ctx (LoadLocal 0) 0
        emit ctx (LoadLocal temp) 0
        emit ctx (SetMember (name, false)) 0
    | GlobalLoc slot -> emit ctx (StoreGlobal slot) 0
    | LocalLoc slot -> emit ctx (StoreLocal slot) 0
    | ConstLoc _ -> ()

let emitExitLoop (ctx: CompileContext) =
    let jumpPc = ctx.Instructions.Count
    emit ctx (Jump -1) 0
    if ctx.ExitLoopPatches.Count > 0 then
        ctx.ExitLoopPatches.[ctx.ExitLoopPatches.Count - 1].Add(jumpPc)

let patchExitLoopTargets (ctx: CompileContext) =
    if ctx.ExitLoopPatches.Count > 0 then
        let patches = ctx.ExitLoopPatches.[ctx.ExitLoopPatches.Count - 1]
        ctx.ExitLoopPatches.RemoveAt(ctx.ExitLoopPatches.Count - 1)
        let target = ctx.Instructions.Count
        for pc in patches do
            patchJump ctx pc (Jump target)

/// Emit an unconditional run-time error.
let private emitRaise (ctx: CompileContext) number (description: string) =
    emitConst ctx (VInteger number)
    emit ctx LoadEmpty 0
    emitConst ctx (VString description)
    emit ctx RaiseErr 0

let private isVariableName (ctx: CompileContext) name =
    (staticKey ctx name).IsSome || isField ctx name || containsName ctx.Locals name || (tryResolveGlobal ctx name).IsSome

let private isModuleName (ctx: CompileContext) name =
    containsSetName ctx.ModuleNames name && not (isVariableName ctx name)

let private isErrName (ctx: CompileContext) (name: string) =
    normalizeName name = "err" && not (isVariableName ctx name)

let private isScalarTypeName (t: string option) =
    match t with
    | Some name -> (ValueSemantics.declaredTypeOf name |> Option.filter (fun dt -> dt <> ValueSemantics.DeclaredType.Object)).IsSome
    | None -> false

/// Whether an expression may evaluate to a host object (and so needs Let-coercion).
let rec private mayBeObject (ctx: CompileContext) expr =
    match expr with
    | Literal _ | Binary _ | Unary _ | TypeOfIs _ | NewExpr _ | MissingArg -> false
    | Parenthesized inner -> mayBeObject ctx inner
    | Identifier name -> not (isScalarTypeName (declaredTypeName ctx name))
    | Ast.Call (Identifier name, _) when containsSetName ctx.ArrayVars name -> not (isScalarTypeName (declaredTypeName ctx name))
    | Ast.Call (Identifier name, _) when not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        // Builtins return scalars, except CreateObject/GetObject.
        match normalizeName name with
        | "createobject" | "getobject" | "iif" | "choose" | "switch" | "callbyname" | "cvar" -> true
        | _ -> false
    | _ -> true

/// The static type of an expression: Variant, or a declared type known at
/// compile time (literals, typed variables, constants, Enum members, typed
/// functions and the built-ins VBA declares with a type).
[<RequireQualifiedAccess>]
type private SType =
    | Variant
    | Typed of ValueSemantics.DeclaredType
    /// Declared, but not one of the scalar types (a user-defined type...).
    | TypedOther

let private isTyped t = match t with SType.Variant -> false | _ -> true

let private literalSType (l: Literal) =
    match l with
    | Integer i when i >= -32768 && i <= 32767 -> SType.Typed ValueSemantics.DeclaredType.Integer
    | Integer _ | Long _ -> SType.Typed ValueSemantics.DeclaredType.Long
    | Single _ -> SType.Typed ValueSemantics.DeclaredType.Single
    | Double _ -> SType.Typed ValueSemantics.DeclaredType.Double
    | Currency _ -> SType.Typed ValueSemantics.DeclaredType.Currency
    | String _ -> SType.Typed ValueSemantics.DeclaredType.String
    | Boolean _ -> SType.Typed ValueSemantics.DeclaredType.Boolean
    | Date _ -> SType.Typed ValueSemantics.DeclaredType.Date
    | Null | Empty | Nothing -> SType.Variant

let private valueSType (v: Value) =
    match v with
    | VInteger i when i >= -32768 && i <= 32767 -> SType.Typed ValueSemantics.DeclaredType.Integer
    | VInteger _ | VLong _ -> SType.Typed ValueSemantics.DeclaredType.Long
    | VString _ -> SType.Typed ValueSemantics.DeclaredType.String
    | VDouble _ -> SType.Typed ValueSemantics.DeclaredType.Double
    | VBoolean _ -> SType.Typed ValueSemantics.DeclaredType.Boolean
    | _ -> SType.Variant

let private typeNameSType (name: string option) =
    match name |> Option.bind ValueSemantics.declaredTypeOf with
    | Some ValueSemantics.DeclaredType.Object -> SType.Variant
    | Some ValueSemantics.DeclaredType.LongLong -> SType.TypedOther
    | Some dt -> SType.Typed dt
    | None -> SType.Variant

/// Numeric precision ranks of declared types (see ValueSemantics).
let private typeRank (dt: ValueSemantics.DeclaredType) =
    match dt with
    | ValueSemantics.DeclaredType.Byte -> 0
    | ValueSemantics.DeclaredType.Integer | ValueSemantics.DeclaredType.Boolean -> 1
    | ValueSemantics.DeclaredType.Long -> 2
    | ValueSemantics.DeclaredType.Single -> 3
    | ValueSemantics.DeclaredType.Double | ValueSemantics.DeclaredType.String -> 4
    | ValueSemantics.DeclaredType.Currency -> 5
    | ValueSemantics.DeclaredType.Date -> 6
    | _ -> 7

let private rankType rank =
    match rank with
    | 0 -> SType.Typed ValueSemantics.DeclaredType.Byte
    | 1 -> SType.Typed ValueSemantics.DeclaredType.Integer
    | 2 -> SType.Typed ValueSemantics.DeclaredType.Long
    | 3 -> SType.Typed ValueSemantics.DeclaredType.Single
    | 4 -> SType.Typed ValueSemantics.DeclaredType.Double
    | 5 -> SType.Typed ValueSemantics.DeclaredType.Currency
    | 6 -> SType.Typed ValueSemantics.DeclaredType.Date
    | _ -> SType.TypedOther

/// The static result type of `left op right` when both are typed.
let private binarySType op (l: SType) (r: SType) =
    match l, r with
    | SType.Typed lt, SType.Typed rt ->
        let lr = typeRank lt
        let rr = typeRank rt
        let whole a b = if a = 0 && b = 0 then rankType 0 elif a <= 1 && b <= 1 then rankType 1 else rankType 2
        match op with
        | Ast.Add | Ast.Subtract | Ast.Multiply ->
            if lr = 7 || rr = 7 then SType.TypedOther
            elif op = Ast.Add && lt = ValueSemantics.DeclaredType.String && rt = ValueSemantics.DeclaredType.String then l
            elif lr = 6 || rr = 6 then
                if op = Ast.Multiply || (op = Ast.Subtract && lr = 6 && rr = 6) then rankType 4 else rankType 6
            elif (lr = 3 && rr = 2) || (lr = 2 && rr = 3) then rankType 4
            elif op = Ast.Multiply && (lr = 5 || rr = 5) && (lr = 3 || rr = 3 || lr = 4 || rr = 4) then rankType 4
            else rankType (max lr rr)
        | Ast.Divide ->
            let singleOperand rank = rank = 0 || rank = 1 || rank = 3
            if (lr = 3 || rr = 3) && singleOperand lr && singleOperand rr then rankType 3 else rankType 4
        | Ast.IntDivide | Ast.Modulus -> whole lr rr
        | Ast.Power -> rankType 4
        | Ast.Concatenate -> SType.Typed ValueSemantics.DeclaredType.String
        | Ast.Equal | Ast.NotEqual | Ast.LessThan | Ast.LessEqual | Ast.GreaterThan | Ast.GreaterEqual
        | Ast.Is | Ast.Like -> SType.Typed ValueSemantics.DeclaredType.Boolean
        | Ast.And | Ast.Or | Ast.Xor | Ast.Eqv | Ast.Imp ->
            if lt = ValueSemantics.DeclaredType.Boolean && rt = ValueSemantics.DeclaredType.Boolean then l
            else whole lr rr
    | SType.Variant, _ | _, SType.Variant -> SType.Variant
    | _ -> SType.TypedOther

/// Built-ins that VBA declares with a type (an expression with them as an
/// operand overflows instead of widening); the others return Variants.
let private builtinSType (name: string) (argType: unit -> SType) =
    let typed dt = SType.Typed dt
    match name with
    | "len" | "lenb" | "instr" | "instrrev" | "ubound" | "lbound" | "vartype" | "rgb" | "qbcolor" | "clng" ->
        typed ValueSemantics.DeclaredType.Long
    | "cint" | "asc" | "ascw" | "sgn" | "strcomp" -> typed ValueSemantics.DeclaredType.Integer
    | "ascb" | "cbyte" -> typed ValueSemantics.DeclaredType.Byte
    | "csng" -> typed ValueSemantics.DeclaredType.Single
    | "cdbl" -> typed ValueSemantics.DeclaredType.Double
    | "ccur" -> typed ValueSemantics.DeclaredType.Currency
    | "cbool" -> typed ValueSemantics.DeclaredType.Boolean
    | "cdate" -> typed ValueSemantics.DeclaredType.Date
    | "cstr" -> typed ValueSemantics.DeclaredType.String
    | "abs" | "int" | "fix" ->
        match argType () with
        | SType.Typed ValueSemantics.DeclaredType.Boolean -> typed ValueSemantics.DeclaredType.Integer
        | SType.Typed ValueSemantics.DeclaredType.String -> typed ValueSemantics.DeclaredType.Double
        | t -> t
    | _ -> SType.Variant

/// `VBA.Left`, `Strings.Left`, `VBA.Strings.Left`, `Math.Sqr`...: the VBA
/// library's module names qualify built-ins (unless the program defines the name).
let private libraryModules = set [ "vba"; "strings"; "math"; "interaction"; "datetime"; "conversion"; "information"; "financial"; "filesystem"; "constants"; "colorconstants"; "keycodeconstants"; "systemcolorconstants" ]

let private libraryMember (ctx: CompileContext) (expr: Expression) =
    let isLibrary name =
        libraryModules.Contains (normalizeName name) && not (isVariableName ctx name) && not (containsSetName ctx.ModuleNames name)
        && (tryResolveFunction ctx name).IsNone && not (containsName ctx.ClassIndex name) && not (containsName ctx.Enums name)
    match expr with
    | Member (Member (Identifier vba, library), name) when normalizeName vba = "vba" && isLibrary vba && isLibrary library -> Some name
    | Member (Identifier library, name) when isLibrary library -> Some name
    | _ -> None

let rec private staticTypeOf (ctx: CompileContext) expr =
    match expr with
    | Member _ when (libraryMember ctx expr).IsSome -> staticTypeOf ctx (Identifier (libraryMember ctx expr).Value)
    | Ast.Call (callee, args) when (libraryMember ctx callee).IsSome -> staticTypeOf ctx (Ast.Call (Identifier (libraryMember ctx callee).Value, args))
    | Literal l -> literalSType l
    | Parenthesized e -> staticTypeOf ctx e
    | Identifier name when isErrName ctx name -> SType.Variant
    | Identifier name when normalizeName name = "erl" && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        SType.Typed ValueSemantics.DeclaredType.Long
    | Identifier name when isVariableName ctx name ->
        if (tryResolveFunction ctx name).IsSome && not (isLocalVariable ctx name) && (tryResolveGlobal ctx name).IsNone then SType.Variant
        else typeNameSType (declaredTypeName ctx name)
    | Identifier name when (tryResolveFunction ctx name).IsSome ->
        let _, index = (tryResolveFunction ctx name).Value
        typeNameSType (match ctx.FunctionTypes.TryGetValue index with | true, t -> Some t | _ -> None)
    | Identifier name ->
        match Builtins.tryConstant name with
        | Some value -> valueSType value
        | None -> SType.Variant
    | Member (Identifier enumName, _) when containsName ctx.Enums enumName && not (isVariableName ctx enumName) ->
        SType.Typed ValueSemantics.DeclaredType.Long
    | Member (Identifier objectName, memberName) when isErrName ctx objectName && normalizeName memberName = "number" ->
        SType.Typed ValueSemantics.DeclaredType.Long
    | Unary (Ast.Negate, e) ->
        match staticTypeOf ctx e with
        | SType.Typed (ValueSemantics.DeclaredType.Byte | ValueSemantics.DeclaredType.Boolean) -> SType.Typed ValueSemantics.DeclaredType.Integer
        | SType.Typed ValueSemantics.DeclaredType.String -> SType.Typed ValueSemantics.DeclaredType.Double
        | t -> t
    | Unary (Ast.Not, e) ->
        match staticTypeOf ctx e with
        | SType.Typed (ValueSemantics.DeclaredType.Byte | ValueSemantics.DeclaredType.Integer | ValueSemantics.DeclaredType.Long | ValueSemantics.DeclaredType.Boolean) as t -> t
        | SType.Variant -> SType.Variant
        | _ -> SType.Typed ValueSemantics.DeclaredType.Long
    | Binary (op, l, r) -> binarySType op (staticTypeOf ctx l) (staticTypeOf ctx r)
    | Ast.Call (Identifier name, args) when containsSetName ctx.ArrayVars name && isVariableName ctx name && not args.IsEmpty ->
        typeNameSType (declaredTypeName ctx name)
    | Ast.Call (Identifier name, args) when not (isVariableName ctx name) ->
        match tryResolveFunction ctx name with
        | Some (_, index) -> typeNameSType (match ctx.FunctionTypes.TryGetValue index with | true, t -> Some t | _ -> None)
        | None ->
            builtinSType (normalizeName name) (fun () ->
                match args with
                | [ arg ] -> staticTypeOf ctx arg
                | _ -> SType.Variant)
    | _ -> SType.Variant

/// The declared type a literal or typed variable already has, if static.
let private staticType (ctx: CompileContext) expr =
    match staticTypeOf ctx expr with
    | SType.Typed dt -> Some [ dt ]
    | _ -> None

/// Push one argument for a by-reference parameter: a reference when the
/// argument is a variable, array element or field, else its value.
let rec private emitRefArgument (ctx: CompileContext) (arg: Expression) (compileExpr: CompileContext -> Expression -> unit) =
    match arg with
    | Identifier varName when isField ctx varName ->
        emit ctx (LoadLocal 0) 0
        emit ctx (MakeRefMember varName) 0
    | Identifier varName when isVariableName ctx varName && not ((tryResolveFunction ctx varName).IsSome && not (isLocalVariable ctx varName) && (tryResolveGlobal ctx varName).IsNone) ->
        emitEnsureNew ctx varName
        match resolveVar ctx varName true with
        | GlobalLoc slot -> emit ctx (MakeRefGlobal slot) 0
        | LocalLoc slot -> emit ctx (MakeRefLocal slot) 0
        | _ -> compileExpr ctx arg
    | Ast.Call (Identifier arrayName, []) when containsSetName ctx.ArrayVars arrayName && isVariableName ctx arrayName ->
        // `Show p()`: the array itself.
        emitRefArgument ctx (Identifier arrayName) compileExpr
    | Ast.Call (Identifier arrayName, indices)
        when isVariableName ctx arrayName && (tryResolveFunction ctx arrayName).IsNone
             && not (isField ctx arrayName) ->
        emitLoadVar ctx arrayName
        for index in indices do compileExpr ctx index
        emit ctx (MakeRefArray indices.Length) 0
    | Member (Identifier objectName, _) when isErrName ctx objectName || isModuleName ctx objectName ->
        compileExpr ctx arg
    | Member (receiver, memberName) when not (match receiver with Identifier n -> containsName ctx.Enums n && not (isVariableName ctx n) | _ -> false) ->
        compileExpr ctx receiver
        emit ctx (MakeRefMember memberName) 0
    | MissingArg -> emitConst ctx VUndefined
    | NamedArg (_, value) -> emitRefArgument ctx value compileExpr
    | _ -> compileExpr ctx arg

let private emitDefaultParameter (ctx: CompileContext) (p: Parameter) (compileExpr: CompileContext -> Expression -> unit) =
    match p.DefaultValue with
    | Some value -> compileExpr ctx value
    | None ->
        match p.Type |> Option.bind (fun t -> ValueSemantics.declaredTypeOf t.Name) with
        | Some dt when not p.IsArray -> emitConst ctx (ValueSemantics.defaultOf dt)
        | _ -> emitConst ctx VUndefined

let private hasNamed (args: Expression list) = args |> List.exists (function NamedArg _ -> true | _ -> false)

/// Place `name:=value` arguments at their parameter's position.
let private reorderNamed (ctx: CompileContext) (paramNames: string list) (args: Expression list) =
    if not (hasNamed args) then List.toArray args
    else
        let names = List.toArray paramNames
        let positional = args |> List.takeWhile (function NamedArg _ -> false | _ -> true) |> List.toArray
        let result = Array.create (max names.Length positional.Length) MissingArg
        Array.blit positional 0 result 0 positional.Length
        for arg in args |> List.skip positional.Length do
            match arg with
            | NamedArg (name, value) ->
                match names |> Array.tryFindIndex (fun n -> normalizeName n = normalizeName name) with
                | Some i -> result.[i] <- value
                | None -> fail ctx (sprintf "Named argument not found: %s" name)
            | _ -> fail ctx "Positional argument after a named argument"
        // Trailing omitted arguments are simply absent.
        let mutable count = result.Length
        while count > positional.Length && result.[count - 1] = MissingArg do count <- count - 1
        result.[.. count - 1]

let private builtinParameterNames name =
    match name with
    | "msgbox" -> [ "Prompt"; "Buttons"; "Title"; "HelpFile"; "Context" ]
    | "inputbox" -> [ "Prompt"; "Title"; "Default"; "XPos"; "YPos"; "HelpFile"; "Context" ]
    | "instr" -> [ "Start"; "String1"; "String2"; "Compare" ]
    | "replace" -> [ "Expression"; "Find"; "Replace"; "Start"; "Count"; "Compare" ]
    | "split" -> [ "Expression"; "Delimiter"; "Limit"; "Compare" ]
    | "strcomp" -> [ "String1"; "String2"; "Compare" ]
    | "round" -> [ "Number"; "NumDigitsAfterDecimal" ]
    | "createobject" -> [ "Class"; "ServerName" ]
    | _ -> []

/// Call a compiled procedure, matching arguments to its declared parameters.
let private emitProcedureCall (ctx: CompileContext) (key: string) (funcIndex: int) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    let parameters =
        match tryGetName ctx.Parameters key with
        | true, ps -> ps
        | false, _ -> []
    let fixedParams = parameters |> List.filter (fun p -> not p.IsParamArray)
    let paramArray = parameters |> List.tryFind (fun p -> p.IsParamArray)
    let args = reorderNamed ctx (parameters |> List.map (fun p -> p.Name)) args
    let fixedCount = fixedParams.Length
    if args.Length > fixedCount && paramArray.IsNone then
        emitRaise ctx 450 (errorDescription 450)
        emit ctx LoadEmpty 0
    else
        let missingRequired =
            fixedParams |> List.mapi (fun i p -> i, p)
            |> List.exists (fun (i, p) -> not p.Optional && (i >= args.Length || args.[i] = MissingArg))
        if missingRequired then
            emitRaise ctx 449 (errorDescription 449)
            emit ctx LoadEmpty 0
        else
            fixedParams |> List.iteri (fun i p ->
                if i < args.Length && args.[i] <> MissingArg then
                    if p.ByRef then emitRefArgument ctx args.[i] compileExpr
                    else compileExpr ctx args.[i]
                else emitDefaultParameter ctx p compileExpr)
            match paramArray with
            | Some _ ->
                let extra = if args.Length > fixedCount then args.[fixedCount ..] else [||]
                for arg in extra do
                    if arg = MissingArg then emitConst ctx VUndefined else compileExpr ctx arg
                emit ctx (CallBuiltin ("array", extra.Length)) 0
            | None -> ()
            emit ctx (Call (funcIndex, parameters.Length)) 0

let private emitArguments (ctx: CompileContext) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    for arg in args do
        match arg with
        | MissingArg -> emitConst ctx VUndefined
        | NamedArg (_, value) -> compileExpr ctx value
        | _ -> compileExpr ctx arg

let emitCall (ctx: CompileContext) (name: string) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    match tryResolveFunction ctx name with
    | Some (key, funcIndex) -> emitProcedureCall ctx key funcIndex args compileExpr
    | None ->
        let args =
            if hasNamed args then reorderNamed ctx (builtinParameterNames (normalizeName name)) args |> Array.toList
            else args
        emitArguments ctx args compileExpr
        // Builtins dispatch case-insensitively; lowercase the name once here so
        // the VM needn't re-lowercase (allocate) on every call.
        emit ctx (CallBuiltin (normalizeName name, List.length args)) 0

let private emitDefaultCall (ctx: CompileContext) (args: Expression list) (compileExpr: CompileContext -> Expression -> unit) =
    emitArguments ctx args compileExpr
    emit ctx (CallDefault (List.length args)) 0

let private binaryOpcode op =
    match op with
    | Ast.Add -> Add
    | Ast.Subtract -> Subtract
    | Ast.Multiply -> Multiply
    | Ast.Divide -> Divide
    | Ast.IntDivide -> Bytecode.IntDivide
    | Ast.Modulus -> Modulus
    | Ast.Power -> Power
    | Ast.Concatenate -> Concat
    | Ast.Equal -> Equal
    | Ast.NotEqual -> NotEqual
    | Ast.LessThan -> LessThan
    | Ast.LessEqual -> LessEqual
    | Ast.GreaterThan -> GreaterThan
    | Ast.GreaterEqual -> GreaterEqual
    | Ast.And -> And
    | Ast.Or -> Or
    | Ast.Xor -> Xor
    | Ast.Is -> IsOp
    | Ast.Like -> LikeOp
    | Ast.Eqv -> Bytecode.Eqv
    | Ast.Imp -> Bytecode.Imp

/// The opcode of an operator whose operands both have declared types:
/// overflow raises error 6 instead of widening.
let private checkedOpcode op =
    match op with
    | Ast.Add -> AddChecked
    | Ast.Subtract -> SubtractChecked
    | Ast.Multiply -> MultiplyChecked
    | Ast.Divide -> DivideChecked
    | Ast.IntDivide -> IntDivideChecked
    | Ast.Modulus -> ModulusChecked
    | other -> binaryOpcode other

let rec compileExpression (ctx: CompileContext) (expr: Expression) =
    match expr with
    | Ast.Call (Identifier "[evaluate]", [ text ]) ->
        // `[A1]` is Evaluate("A1"): a host global or procedure Evaluate, else
        // Application.Evaluate.
        if isVariableName ctx "Evaluate" || (tryResolveFunction ctx "Evaluate").IsSome then
            compileExpression ctx (Ast.Call (Identifier "Evaluate", [ text ]))
        elif isVariableName ctx "Application" then
            compileExpression ctx (Ast.Call (Member (Identifier "Application", "Evaluate"), [ text ]))
        else
            emitRaise ctx 424 (errorDescription 424)
            emit ctx LoadEmpty 0
    | Member _ when (libraryMember ctx expr).IsSome -> compileExpression ctx (Identifier (libraryMember ctx expr).Value)
    | Ast.Call (callee, args) when (libraryMember ctx callee).IsSome -> compileExpression ctx (Ast.Call (Identifier (libraryMember ctx callee).Value, args))
    | Parenthesized e -> compileExpression ctx e
    | ArrayBound _ -> fail ctx "Array bound outside declaration"
    | MissingArg -> emitConst ctx VUndefined
    | NamedArg (name, _) -> fail ctx (sprintf "Named argument not expected here: %s" name)
    | Literal (Integer i) -> emitConst ctx (if i >= -32768 && i <= 32767 then VInteger i else VLong i)
    | Literal (Long i) -> emitConst ctx (VLong i)
    | Literal (Single f) -> emitConst ctx (VSingle f)
    | Literal (Currency c) -> emitConst ctx (VCurrency c)
    | Literal (Double d) -> emitConst ctx (VDouble d)
    | Literal (String s) -> emitConst ctx (VString s)
    | Literal (Boolean b) -> if b then emit ctx LoadTrue 0 else emit ctx LoadFalse 0
    | Literal Null -> emit ctx LoadNull 0
    | Literal Empty -> emit ctx LoadEmpty 0
    | Literal Nothing -> emitConst ctx VNothing
    | Literal (Date d) -> emitConst ctx (VDate d)
    | Identifier name when isErrName ctx name -> emit ctx LoadErrObject 0
    | Identifier name when normalizeName name = "erl" && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        emit ctx LoadErl 0
    | Identifier name when Builtins.isArgumentlessBuiltin name && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        emit ctx (CallBuiltin (normalizeName name, 0)) 0
    | Identifier name when not (isVariableName ctx name) && (tryResolveFunction ctx name).IsSome ->
        // A parameterless function used as a value.
        emitCall ctx name [] compileExpression
    | Identifier name -> emitLoadVar ctx name
    | Unary (Ast.Not, e) ->
        compileExpression ctx e
        emit ctx Bytecode.Not 0
    | Unary (Ast.Negate, e) ->
        compileExpression ctx e
        emit ctx (if isTyped (staticTypeOf ctx e) then NegateChecked else Bytecode.Negate) 0
    | Binary (op, left, right) ->
        compileExpression ctx left
        compileExpression ctx right
        let strict = isTyped (staticTypeOf ctx left) && isTyped (staticTypeOf ctx right)
        emit ctx (if strict then checkedOpcode op else binaryOpcode op) 0
    | Ast.Call (Identifier name, args) when
        (match ctx.ClassName with Some className -> containsName ctx.FunctionIndex (className + "." + name) | None -> false) ->
        compileExpression ctx (Ast.Call (Member (MeExpr, name), args))
    | Ast.Call (Identifier name, args) when normalizeName name = "erl" && args.IsEmpty && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        emit ctx LoadErl 0
    | Ast.Call (Identifier name, args) when normalizeName name = "callbyname" && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        match args with
        | target :: procName :: callType :: rest ->
            compileExpression ctx target
            compileExpression ctx procName
            compileExpression ctx callType
            emitArguments ctx rest compileExpression
            emit ctx (CallByNameOp rest.Length) 0
        | _ ->
            emitRaise ctx 449 (errorDescription 449)
            emit ctx LoadEmpty 0
    | Ast.Call (Identifier name, args) when isField ctx name ->
        emit ctx (LoadLocal 0) 0
        emit ctx (GetMember name) 0
        emitDefaultCall ctx args compileExpression
    | Ast.Call (Identifier name, args) ->
        let isKnownValue = containsSetName ctx.ArrayVars name || isVariableName ctx name
        let isFunction = (tryResolveFunction ctx name).IsSome
        // Inside Function F, F(x) is a recursive call unless F is an array.
        // A local (or parameter) shadows a procedure of the same name.
        if isKnownValue && (not isFunction || (isLocalVariable ctx name && ctx.ReturnSlotName <> Some name)) then
            emitLoadVar ctx name
            if containsSetName ctx.ArrayVars name && args.IsEmpty then ()
            elif containsSetName ctx.ArrayVars name && not (hasNamed args) && not (List.contains MissingArg args) then
                for arg in args do compileExpression ctx arg
                emit ctx (ArrayGet args.Length) 0
            else
                emitDefaultCall ctx args compileExpression
        else
            emitCall ctx name args compileExpression
    | Ast.Call (Member (Identifier objName, methodName), args) when normalizeName objName = "wscript" && not (isVariableName ctx objName) ->
        match normalizeName methodName with
        | "echo" ->
            emitArguments ctx args compileExpression
            emit ctx (CallBuiltin ("print", List.length args)) 0
        | "quit" -> emit ctx Halt 0
        | _ -> emit ctx LoadEmpty 0
    | Ast.Call (Member (Identifier objName, methodName), args) when normalizeName objName = "debug" && not (isVariableName ctx objName) ->
        match normalizeName methodName with
        | "print" | "print;" ->
            emitArguments ctx args compileExpression
            emit ctx (CallBuiltin ("debug." + normalizeName methodName, List.length args)) 0
        | _ ->
            // Debug.Assert and friends: evaluate the arguments, no effect.
            for arg in args do
                compileExpression ctx arg
                emit ctx Pop 0
            emit ctx LoadEmpty 0
    | Ast.Call (Member (Identifier objName, methodName), args) when isErrName ctx objName ->
        match normalizeName methodName with
        | "clear" -> emit ctx ClearErr 0
        | "raise" ->
            let args = List.toArray args
            let argOr i = if i < args.Length && args.[i] <> MissingArg then compileExpression ctx args.[i] else emitConst ctx VUndefined
            if args.Length = 0 then emitConst ctx (VInteger 5) else argOr 0
            argOr 1
            argOr 2
            emit ctx RaiseErr 0
        | _ ->
            emit ctx LoadErrObject 0
            emitArguments ctx args compileExpression
            emit ctx (CallMethod (methodName, List.length args)) 0
    | Ast.Call (Member (Identifier moduleName, procName), args) when isModuleName ctx moduleName ->
        let key = privateKey moduleName procName
        match tryGetName ctx.FunctionIndex key with
        | true, funcIndex -> emitProcedureCall ctx key funcIndex args compileExpression
        | _ ->
            compileExpression ctx (Member (Identifier moduleName, procName))
            emitDefaultCall ctx args compileExpression
    | Ast.Call (Member (objExpr, methodName), args) ->
        compileExpression ctx objExpr
        // Arguments to methods are passed by reference where possible; the
        // callee's ByVal parameters copy the value on entry.
        for arg in args do emitRefArgument ctx arg compileExpression
        if hasNamed args then
            let names = args |> List.map (function NamedArg (n, _) -> n | _ -> "") |> List.toArray
            emit ctx (CallMethodNamed (methodName, names)) 0
        else
            emit ctx (CallMethod (methodName, List.length args)) 0
    | Ast.Call (callee, args) ->
        // The result of a call, member or expression indexed again: Split(s)(1).
        compileExpression ctx callee
        emitDefaultCall ctx args compileExpression
    | MeExpr ->
        // Slot 0 in class methods is reserved for Me
        emit ctx (LoadLocal 0) 0
    | TypeOfIs (expr, className) ->
        compileExpression ctx expr
        emit ctx (TypeCheck className) 0
    | NewExpr className -> emitNew ctx className
    | WithDotExpr ->
        match ctx.WithTargetSlot with
        | Some slot -> emit ctx (LoadLocal slot) 0
        | None -> fail ctx "Invalid or unqualified reference"
    | Member (Identifier objName, memberName) when isErrName ctx objName ->
        match normalizeName memberName with
        | "number" -> emit ctx LoadErrNumber 0
        | "description" -> emit ctx LoadErrDescription 0
        | "source" -> emit ctx LoadErrSource 0
        | "clear" -> emit ctx ClearErr 0
        | _ ->
            emit ctx LoadErrObject 0
            emit ctx (GetMember memberName) 0
    | Member (Identifier enumName, memberName) when containsName ctx.Enums enumName && not (isVariableName ctx enumName) ->
        if not (containsSetName (getName ctx.Enums enumName) memberName) then
            fail ctx (sprintf "Method or data member not found: %s.%s" enumName memberName)
        emitLoadVar ctx memberName
    | Member (Identifier moduleName, memberName) when isModuleName ctx moduleName ->
        match tryGetName ctx.Globals (privateKey moduleName memberName) with
        | true, slot -> emit ctx (LoadGlobal slot) 0
        | _ ->
            match tryGetName ctx.FunctionIndex (privateKey moduleName memberName) with
            | true, funcIndex -> emitProcedureCall ctx (privateKey moduleName memberName) funcIndex [] compileExpression
            | _ ->
                match tryGetName ctx.Globals memberName with
                | true, slot -> emit ctx (LoadGlobal slot) 0
                | _ -> fail ctx (sprintf "Method or data member not found: %s.%s" moduleName memberName)
    | Member (objExpr, memberName) ->
        compileExpression ctx objExpr
        emit ctx (GetMember memberName) 0

let private hasConversion (ctx: CompileContext) name =
    match declaredTypeName ctx name with
    | Some t -> (ValueSemantics.declaredTypeOf t).IsSome
    | None -> false

let private storeRaw (ctx: CompileContext) name =
    match resolveVar ctx name true with
    | FieldLoc ->
        let temp = freshLocal ctx
        emit ctx (StoreLocal temp) 0
        emit ctx (LoadLocal 0) 0
        emit ctx (LoadLocal temp) 0
        emit ctx (SetMember (name, false)) 0
    | GlobalLoc slot -> emit ctx (StoreGlobal slot) 0
    | LocalLoc slot -> emit ctx (StoreLocal slot) 0
    | ConstLoc _ -> ()

/// Compile the right-hand side of a Let assignment.
let private compileLetValue (ctx: CompileContext) (expr: Expression) (convertsOnStore: bool) =
    compileExpression ctx expr
    if mayBeObject ctx expr && not convertsOnStore then emit ctx LetValue 0

/// Compile a value for a typed variable, skipping a conversion that the
/// value's static type makes unnecessary.
let private compileTypedValue (ctx: CompileContext) (expr: Expression) (name: string) =
    let target = declaredTypeName ctx name
    let rec literalValue e =
        match e with
        | Literal (Integer i) -> Some (if i >= -32768 && i <= 32767 then VInteger i else VLong i)
        | Literal (Long i) -> Some (VLong i)
        | Literal (Single f) -> Some (VSingle f)
        | Literal (Double d) -> Some (VDouble d)
        | Literal (Currency c) -> Some (VCurrency c)
        | Parenthesized inner -> literalValue inner
        | _ -> None
    let converted =
        match target |> Option.bind ValueSemantics.declaredTypeOf, literalValue expr with
        | Some dt, Some value when dt <> ValueSemantics.DeclaredType.Object ->
            // A literal converts at compile time (`Dim l As Long: l = 0`).
            try Some (ValueSemantics.coerceTo dt value) with _ -> None
        | _ -> None
    match target |> Option.bind ValueSemantics.declaredTypeOf, staticType ctx expr with
    | _ when converted.IsSome ->
        emitConst ctx converted.Value
        storeRaw ctx name
    | Some dt, Some kinds when List.contains dt kinds ->
        compileExpression ctx expr
        storeRaw ctx name
    | _ ->
        compileLetValue ctx expr (hasConversion ctx name)
        storeName ctx name

/// Initial value for a declaration `As typeName` (nothing for Variant).
let private emitTypedInitial (ctx: CompileContext) (typeName: string) =
    match ValueSemantics.declaredTypeOf typeName with
    | Some dt -> emitConst ctx (ValueSemantics.defaultOf dt); true
    | None ->
        match udtIndex ctx typeName with
        | Some idx -> emit ctx (NewObj idx) 0; true
        | None -> false

/// Fill a just-created array with the element default of `typeName`.
let private emitArrayFill (ctx: CompileContext) (typeName: string option) =
    match typeName with
    | Some t ->
        match ValueSemantics.declaredTypeOf t with
        | Some dt -> emit ctx (ArrayFill (addConstant ctx (ValueSemantics.defaultOf dt))) 0
        | None ->
            match udtIndex ctx t with
            | Some idx -> emit ctx (ArrayFillNew idx) 0
            | None -> ()
    | None -> ()

let private emitDimensions (ctx: CompileContext) (dimensions: Expression list) =
    for dimension in dimensions do
        match dimension with
        | ArrayBound (lo, hi) -> compileExpression ctx lo; compileExpression ctx hi
        | upper -> emitConst ctx (VInteger ctx.OptionBase); compileExpression ctx upper

/// `Let`-assignments to Variant and object variables go through the
/// default member when the variable holds an object (`r = 5` with r a Range).
let private letsThroughObject (ctx: CompileContext) name =
    match declaredTypeName ctx name with
    | None -> true
    | Some t ->
        match ValueSemantics.declaredTypeOf t with
        | Some dt -> dt = ValueSemantics.DeclaredType.Object
        | None -> (udtIndex ctx t).IsNone

/// The statement assigning `value` to an assignable expression.
let private assignmentTo (ctx: CompileContext) (target: Expression) (value: Expression) =
    match target with
    | Identifier name -> Assignment (name, value)
    | Ast.Call (Identifier name, indices) -> IndexedAssignment (name, indices, value, false)
    | Member (receiver, name) -> MemberAssignment (receiver, name, value, false)
    | Ast.Call (Member _, _) -> DefaultAssignment (target, value, false)
    | Parenthesized inner -> DefaultAssignment (inner, value, false)
    | _ -> fail ctx "Expected a variable"

let private eventScope (ctx: CompileContext) =
    match ctx.ClassFields, ctx.ClassName with
    | Some _, Some className -> className
    | _ -> ctx.ModuleName

let private tryEventBinding (ctx: CompileContext) name =
    match tryGetName ctx.WithEventsVars (eventScope ctx + "." + name) with
    | true, index -> Some index
    | _ -> None

let private emitArrayReset (ctx: CompileContext) (typeName: string option) =
    let code =
        match typeName with
        | Some t ->
            match ValueSemantics.declaredTypeOf t with
            | Some dt -> addConstant ctx (ValueSemantics.defaultOf dt)
            | None ->
                match udtIndex ctx t with
                | Some idx -> -(idx + 1)
                | None -> System.Int32.MinValue
        | None -> System.Int32.MinValue
    emit ctx (ArrayReset code) 0

let rec compileStatement (ctx: CompileContext) (stmt: Statement) =
    match stmt with
    | LocatedStmt (line, inner) ->
        let outerLine = ctx.CurrentLine
        ctx.CurrentLine <- line
        let start = ctx.Instructions.Count
        emit ctx (Statement -1) line
        compileStatement ctx inner
        patchJump ctx start (Statement ctx.Instructions.Count)
        ctx.CurrentLine <- outerLine
    | ExpressionStmt expr ->
        compileExpression ctx expr
        emit ctx Pop 0

    | StatementGroup statements -> for inner in statements do compileStatement ctx inner

    | Assignment (name, expr) | Let (name, expr) ->
        match ctx.ReturnSlotName with
        | Some returnName when normalizeName returnName = normalizeName name && ctx.InFunction ->
            compileTypedValue ctx expr name
        | _ when (normalizeName name = "date" || normalizeName name = "time") && not (isVariableName ctx name) ->
            // Setting the system date or time is ignored.
            compileExpression ctx expr
            emit ctx Pop 0
        | _ ->
        match resolveVar ctx name true with
        | FieldLoc ->
            emit ctx (LoadLocal 0) 0
            compileLetValue ctx expr false
            emit ctx (SetMember (name, false)) 0
        | _ when containsSetName ctx.ArrayVars name && (match declaredTypeName ctx name with Some t -> (udtIndex ctx t).IsNone | None -> false) ->
            // `arr = Split(...)` replaces a typed dynamic array.
            compileExpression ctx expr
            storeRaw ctx name
        | GlobalLoc slot when letsThroughObject ctx name ->
            emitEnsureNew ctx name
            compileLetValue ctx expr false
            emit ctx (LetGlobal slot) 0
        | LocalLoc slot when letsThroughObject ctx name ->
            emitEnsureNew ctx name
            compileLetValue ctx expr false
            emit ctx (LetLocal slot) 0
        | _ -> compileTypedValue ctx expr name

    | IndexedAssignment (name, (target :: start :: rest as args), valueExpr, false)
        when normalizeName name = "mid" && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        // Mid(target, start[, length]) = value
        let length = match rest with [ l ] -> l | _ -> MissingArg
        ignore args
        compileStatement ctx (assignmentTo ctx target (Ast.Call (Identifier "mid:statement", [ target; start; length; valueExpr ])))

    | IndexedAssignment ("[evaluate]", args, valueExpr, isSet) ->
        // `[A1] = 10` assigns through the evaluated object's default member.
        compileStatement ctx (DefaultAssignment (Ast.Call (Identifier "[evaluate]", args), valueExpr, isSet))

    | IndexedAssignment (name, indices, valueExpr, true) ->
        // `Set a(i) = obj` stores the object itself.
        emitLoadVar ctx name
        for index in indices do compileExpression ctx index
        compileExpression ctx valueExpr
        emit ctx (ArraySet indices.Length) 0

    | IndexedAssignment (name, indices, valueExpr, false) ->
        emitLoadVar ctx name
        for index in indices do compileExpression ctx index
        // Elements of typed arrays convert; `d(k) = v` on an object does not.
        let elementType =
            if containsSetName ctx.ArrayVars name then declaredTypeName ctx name
            else None
        let elementType = elementType |> Option.filter (fun t -> ValueSemantics.declaredTypeOf t <> Some ValueSemantics.DeclaredType.Object)
        let converts = match elementType with Some t -> (ValueSemantics.declaredTypeOf t).IsSome | None -> false
        compileLetValue ctx valueExpr converts
        emitStoreConversion ctx elementType
        emit ctx (ArraySet indices.Length) 0

    | DefaultAssignment (Ast.Call (Member (receiver, memberName), args), valueExpr, isSet)
        when not (match receiver with Identifier n -> isErrName ctx n || isModuleName ctx n | _ -> false) && not (hasNamed args) ->
        // obj.Name(args) = value: Property Let with arguments, or the item's default member.
        compileExpression ctx receiver
        emitArguments ctx args compileExpression
        if isSet then compileExpression ctx valueExpr else compileLetValue ctx valueExpr false
        emit ctx (SetMemberIndexed (memberName, args.Length)) 0

    | DefaultAssignment (target, valueExpr, isSet) ->
        compileExpression ctx target
        if isSet then compileExpression ctx valueExpr else compileLetValue ctx valueExpr false
        emit ctx SetDefault 0

    | MemberAssignment (objExpr, memberName, valueExpr, isSet) ->
        match objExpr with
        | Identifier moduleName when isModuleName ctx moduleName ->
            match tryGetName ctx.Globals (privateKey moduleName memberName) with
            | true, slot ->
                compileExpression ctx valueExpr
                emit ctx (StoreGlobal slot) 0
            | _ ->
                match tryGetName ctx.Globals memberName with
                | true, slot ->
                    compileExpression ctx valueExpr
                    emit ctx (StoreGlobal slot) 0
                | _ -> fail ctx (sprintf "Method or data member not found: %s.%s" moduleName memberName)
        | _ ->
            compileExpression ctx objExpr
            if isSet then compileExpression ctx valueExpr else compileLetValue ctx valueExpr false
            emit ctx (SetMember (memberName, isSet)) 0

    | Ast.Set (name, expr) ->
        let binding = tryEventBinding ctx name
        // The old value, for Class_Terminate once it is unreachable.
        if ctx.HasTerminate then emitRawLoad ctx name
        let bindEvents () =
            match binding with
            | Some index ->
                emitRawLoad ctx name
                if ctx.EventBindings.[index].ClassSink then emit ctx (LoadLocal 0) 0 else emit ctx LoadEmpty 0
                emit ctx (BindEvents index) 0
            | None -> ()
        match resolveVar ctx name true with
        | FieldLoc ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            bindEvents ()
            emit ctx (SetMember (name, true)) 0
        | _ ->
            compileExpression ctx expr
            bindEvents ()
            storeRaw ctx name
        if ctx.HasTerminate then emit ctx ReleaseCheck 0

    | Declaration (_, StaticDim declarators) when ctx.InFunction ->
        // Initialised once, when the program starts.
        ignore declarators
    | Declaration (vis, StaticDim declarators) -> compileStatement ctx (Declaration (vis, Dim declarators))
    | Declaration (_, Dim declarators) ->
        for d in declarators do
          if (staticKey ctx d.Name).IsNone then
            let typeName = d.Type |> Option.map (fun t -> t.Name)
            let moduleLevel = not ctx.InFunction && ctx.ClassFields.IsNone
            match d.ArraySpec with
            | Some spec when not spec.Dimensions.IsEmpty ->
                emitDimensions ctx spec.Dimensions
                emit ctx (ArrayNew spec.Dimensions.Length) 0
                emitArrayFill ctx typeName
                storeRaw ctx d.Name
                addSetName ctx.ArrayVars d.Name |> ignore
                addSetName ctx.FixedArrays d.Name |> ignore
            | Some _ ->
                addSetName ctx.ArrayVars d.Name |> ignore
                // `Dim a()` is an array without elements (IsArray is True).
                if moduleLevel then
                    emit ctx (CallBuiltin ("array:unallocated", 0)) 0
                    storeRaw ctx d.Name
            | None ->
                // Procedure-level declarations are initialised in the prologue;
                // module-level ones once, here.
                if moduleLevel then
                    if d.IsNew then
                        emitConst ctx VNothing
                        storeRaw ctx d.Name
                    else
                    match typeName with
                    | Some t -> if emitTypedInitial ctx t then storeRaw ctx d.Name
                    | None -> ()

    | Declaration (_, Const (name, _, expr)) ->
        compileExpression ctx expr
        storeName ctx name

    | Declaration (_, ReDim (name, dimensions, preserve)) ->
        ctx.ImplicitAllowed <- true
        if preserve then emitLoadVar ctx name
        emitDimensions ctx dimensions
        emit ctx (if preserve then ReDimPreserve dimensions.Length else ArrayNew dimensions.Length) 0
        emitArrayFill ctx (declaredTypeName ctx name)
        storeRaw ctx name
        ctx.ImplicitAllowed <- false
        addSetName ctx.ArrayVars name |> ignore

    | IfStmt (condition, thenStmts, elseIfs, elseStmtsOpt) ->
        compileExpression ctx condition
        let mutable falseJump = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        for stmt in thenStmts do compileStatement ctx stmt
        let endJumps = ResizeArray<int>()
        for (elseIfCond, elseIfBody) in elseIfs do
            let jump = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add jump
            patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
            compileExpression ctx elseIfCond
            falseJump <- ctx.Instructions.Count
            emit ctx (JumpIfFalse -1) 0
            for stmt in elseIfBody do compileStatement ctx stmt
        match elseStmtsOpt with
        | Some body ->
            let jump = ctx.Instructions.Count
            emit ctx (Jump -1) 0
            endJumps.Add jump
            patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
            for stmt in body do compileStatement ctx stmt
        | None -> patchJump ctx falseJump (JumpIfFalse ctx.Instructions.Count)
        for jump in endJumps do patchJump ctx jump (Jump ctx.Instructions.Count)

    | ForLoop (varName, startExpr, endExpr, stepOpt, body) ->
        compileTypedValue ctx startExpr varName
        let endSlot = freshLocal ctx
        compileExpression ctx endExpr
        emit ctx (StoreLocal endSlot) 0
        let stepSlot = freshLocal ctx
        match stepOpt with
        | Some step -> compileExpression ctx step
        | None -> emitConst ctx (VInteger 1)
        emit ctx (StoreLocal stepSlot) 0
        let counterTyped = isTyped (typeNameSType (declaredTypeName ctx varName))
        if not counterTyped then
            // A Variant counter takes the widest type of start, end and step.
            emitLoadVar ctx varName
            emit ctx (LoadLocal endSlot) 0
            emit ctx (LoadLocal stepSlot) 0
            emit ctx (CallBuiltin ("for:start", 3)) 0
            storeName ctx varName
        let loopStartPc = ctx.Instructions.Count
        emit ctx (LoadLocal stepSlot) 0
        emitConst ctx (VInteger 0)
        emit ctx LessThan 0
        let positiveJump = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        emitLoadVar ctx varName
        emit ctx (LoadLocal endSlot) 0
        emit ctx LessThan 0
        let joinJump = ctx.Instructions.Count
        emit ctx (Jump -1) 0
        patchJump ctx positiveJump (JumpIfFalse ctx.Instructions.Count)
        emitLoadVar ctx varName
        emit ctx (LoadLocal endSlot) 0
        emit ctx GreaterThan 0
        patchJump ctx joinJump (Jump ctx.Instructions.Count)
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0
        ctx.ExitLoopPatches.Add(ResizeArray())
        for stmt in body do compileStatement ctx stmt
        emitLoadVar ctx varName
        emit ctx (LoadLocal stepSlot) 0
        // A typed counter overflows (error 6); a Variant counter widens.
        emit ctx (if counterTyped then AddChecked else Add) 0
        storeName ctx varName
        emit ctx (Jump loopStartPc) 0
        patchJump ctx exitJumpPc (JumpIfTrue ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | ForEach (varName, collExpr, body) ->
        compileExpression ctx collExpr
        emit ctx ForEachPrepare 0
        let collSlot = freshLocal ctx
        emit ctx (StoreLocal collSlot) 0
        let idxSlot = freshLocal ctx
        emitConst ctx (VInteger 0)
        emit ctx (StoreLocal idxSlot) 0

        let loopStartPc = ctx.Instructions.Count
        emit ctx (LoadLocal idxSlot) 0
        emit ctx (LoadLocal collSlot) 0
        emit ctx ArrayLength 0
        emit ctx GreaterEqual 0
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0

        emit ctx (LoadLocal collSlot) 0
        emit ctx (LoadLocal idxSlot) 0
        emit ctx ArrayGetFlat 0
        storeRaw ctx varName

        ctx.ExitLoopPatches.Add(ResizeArray())
        for stmt in body do compileStatement ctx stmt

        emit ctx (LoadLocal idxSlot) 0
        emitConst ctx (VInteger 1)
        emit ctx Add 0
        emit ctx (StoreLocal idxSlot) 0
        emit ctx (Jump loopStartPc) 0
        patchJump ctx exitJumpPc (JumpIfTrue ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | WhileLoop (condition, body) ->
        let loopStartPc = ctx.Instructions.Count
        compileExpression ctx condition
        let exitJumpPc = ctx.Instructions.Count
        emit ctx (JumpIfFalse -1) 0
        ctx.ExitLoopPatches.Add(ResizeArray())
        for stmt in body do compileStatement ctx stmt
        emit ctx (Jump loopStartPc) 0
        patchJump ctx exitJumpPc (JumpIfFalse ctx.Instructions.Count)
        patchExitLoopTargets ctx

    | DoLoop (condOpt, bodyOpt) ->
        ctx.ExitLoopPatches.Add(ResizeArray())
        match condOpt, bodyOpt with
        | Some dc, None ->
            let loopStartPc = ctx.Instructions.Count
            compileExpression ctx dc.Condition
            let exitJumpPc = ctx.Instructions.Count
            emit ctx (JumpIfFalse -1) 0
            for stmt in dc.Body do compileStatement ctx stmt
            emit ctx (Jump loopStartPc) 0
            patchJump ctx exitJumpPc (JumpIfFalse ctx.Instructions.Count)
        | None, Some dc ->
            let loopStartPc = ctx.Instructions.Count
            for stmt in dc.Body do compileStatement ctx stmt
            compileExpression ctx dc.Condition
            emit ctx (JumpIfTrue loopStartPc) 0
        | _ -> ()
        patchExitLoopTargets ctx

    | SelectCase (expr, cases) ->
        compileExpression ctx expr
        let tempSlot = freshLocal ctx
        emit ctx (StoreLocal tempSlot) 0
        let loadTemp () = emit ctx (LoadLocal tempSlot) 0
        let endJumps = ResizeArray<int>()
        for (caseTests, caseBody) in cases do
            match caseTests with
            | None ->
                for stmt in caseBody do compileStatement ctx stmt
            | Some tests ->
                let mutable firstTest = true
                for test in tests do
                    match test with
                    | CaseValue expr ->
                        loadTemp ()
                        compileExpression ctx expr
                        emit ctx Equal 0
                    | CaseRange (lo, hi) ->
                        loadTemp ()
                        compileExpression ctx lo
                        emit ctx GreaterEqual 0
                        loadTemp ()
                        compileExpression ctx hi
                        emit ctx LessEqual 0
                        emit ctx And 0
                    | CaseComparison (op, expr) ->
                        loadTemp ()
                        compileExpression ctx expr
                        match op with
                        | Ast.LessThan -> emit ctx LessThan 0
                        | Ast.LessEqual -> emit ctx LessEqual 0
                        | Ast.GreaterThan -> emit ctx GreaterThan 0
                        | Ast.GreaterEqual -> emit ctx GreaterEqual 0
                        | Ast.NotEqual -> emit ctx NotEqual 0
                        | _ -> emit ctx Equal 0
                    if not firstTest then emit ctx Or 0
                    firstTest <- false
                let skipPc = ctx.Instructions.Count
                emit ctx (JumpIfFalse -1) 0
                for stmt in caseBody do compileStatement ctx stmt
                let endJumpPc = ctx.Instructions.Count
                emit ctx (Jump -1) 0
                endJumps.Add(endJumpPc)
                patchJump ctx skipPc (JumpIfFalse ctx.Instructions.Count)
        let finalTarget = ctx.Instructions.Count
        for jumpPc in endJumps do patchJump ctx jumpPc (Jump finalTarget)

    | ExitFor | ExitDo ->
        emitExitLoop ctx

    | ExitSub | ExitFunction | ExitProperty ->
        match ctx.ReturnSlotName with
        | Some name ->
            emit ctx (LoadLocal (getName ctx.Locals name)) 0
            emit ctx Return 0
        | None -> emit ctx Halt 0

    | CallStmt (Identifier name, []) when normalizeName name = "stop" && not (isVariableName ctx name) && (tryResolveFunction ctx name).IsNone ->
        emit ctx StopOp 0
    | CallStmt (Identifier name, args) ->
        compileExpression ctx (Ast.Call (Identifier name, args))
        emit ctx Pop 0
    | CallStmt (expr, args) ->
        if args.IsEmpty then compileExpression ctx expr else compileExpression ctx (Ast.Call (expr, args))
        emit ctx Pop 0

    | GoToStmt label ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (Jump -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | GoSubStmt label ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (Bytecode.GoSub -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | ReturnStmt -> emit ctx ReturnSub 0

    | LabelStmt label ->
        setName ctx.Labels label ctx.Instructions.Count
        if label.Length > 0 && label.Length < 10 && Seq.forall System.Char.IsDigit label then
            let erl = match ctx.ErlLines.TryGetValue label with | true, n -> n | _ -> int label
            emit ctx (LineLabel erl) 0

    | OnError ResumeNext -> emit ctx OnErrorResumeNext 0
    | OnError GoToZero -> emit ctx OnErrorGoToZero 0
    | OnError GoToMinusOne -> emit ctx OnErrorGoToMinusOne 0
    | OnError (GoToLabel label) ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (OnErrorGoToLabel -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | ResumeStmt ResumeRetry -> emit ctx (Resume 0) 0
    | ResumeStmt ResumeNextStatement -> emit ctx (Resume 1) 0
    | ResumeStmt (ResumeAtLabel label) ->
        let jumpPc = ctx.Instructions.Count
        emit ctx (ResumeLabel -1) 0
        ctx.LabelPatches.Add(label, jumpPc)

    | WithStmt (target, body) ->
        let outerWith = ctx.WithTargetSlot
        compileExpression ctx target
        let slot = freshLocal ctx
        emit ctx (StoreLocal slot) 0
        ctx.WithTargetSlot <- Some slot
        for stmt in body do compileStatement ctx stmt
        ctx.WithTargetSlot <- outerWith

    | EraseStmt names ->
        for name in names do
            if containsSetName ctx.FixedArrays name then
                // A fixed-size array keeps its bounds; its elements are reset.
                emitLoadVar ctx name
                emitArrayReset ctx (declaredTypeName ctx name)
            else
                // A dynamic array is deallocated.
                emit ctx (CallBuiltin ("array:unallocated", 0)) 0
                storeRaw ctx name

    | StopStmt -> emit ctx StopOp 0
    | EndStmt -> emit ctx EndOp 0

    | RaiseEventStmt (eventName, args) ->
        if ctx.ClassFields.IsNone then fail ctx "RaiseEvent is only valid in a class module"
        let handlers = freshLocal ctx
        let index = freshLocal ctx
        emit ctx (LoadLocal 0) 0
        emit ctx (EventHandlers (normalizeName eventName)) 0
        emit ctx (StoreLocal handlers) 0
        emitConst ctx (VInteger 0)
        emit ctx (StoreLocal index) 0
        let loopStart = ctx.Instructions.Count
        emit ctx (LoadLocal index) 0
        emit ctx (LoadLocal handlers) 0
        emit ctx ArrayLength 0
        emit ctx GreaterEqual 0
        let exitJump = ctx.Instructions.Count
        emit ctx (JumpIfTrue -1) 0
        emit ctx (LoadLocal handlers) 0
        emit ctx (LoadLocal index) 0
        emit ctx ArrayGetFlat 0
        for arg in args do emitRefArgument ctx arg compileExpression
        emit ctx (CallHandler args.Length) 0
        emit ctx (LoadLocal index) 0
        emitConst ctx (VInteger 1)
        emit ctx Add 0
        emit ctx (StoreLocal index) 0
        emit ctx (Jump loopStart) 0
        patchJump ctx exitJump (JumpIfTrue ctx.Instructions.Count)

let patchLabels (ctx: CompileContext) =
    for (label, pc) in ctx.LabelPatches do
        match tryGetName ctx.Labels label with
        | true, target ->
            let instr = ctx.Instructions.[pc]
            let newOpcode =
                match instr.Opcode with
                | Jump _ -> Jump target
                | Bytecode.GoSub _ -> Bytecode.GoSub target
                | OnErrorGoToLabel _ -> OnErrorGoToLabel target
                | ResumeLabel _ -> ResumeLabel target
                | other -> other
            patchJump ctx pc newOpcode
        | false, _ ->
            raise (CompileFailure (sprintf "Label not defined: %s" label, ctx.Instructions.[pc].LineNumber, ctx.ModuleName))

/// The type of a constant: its `As` type, or that of its literal value
/// (`Const K = 5` is an Integer).
let private constTypeName (typeRef: TypeRef option) (value: Expression) =
    match typeRef with
    | Some t -> Some t.Name
    | None ->
        let rec literalOf e =
            match e with
            | Literal l -> Some l
            | Parenthesized inner | Unary (Ast.Negate, inner) -> literalOf inner
            | _ -> None
        match literalOf value with
        | Some (Integer i) when i >= -32768 && i <= 32767 -> Some "Integer"
        | Some (Integer _ | Long _) -> Some "Long"
        | Some (Single _) -> Some "Single"
        | Some (Double _) -> Some "Double"
        | Some (Currency _) -> Some "Currency"
        | Some (String _) -> Some "String"
        | Some (Boolean _) -> Some "Boolean"
        | Some (Date _) -> Some "Date"
        | _ -> None

/// The Erl each numeric line label of a procedure reports. VBA reports a
/// line's own number, with one exception observed in Excel 2019: when the
/// procedure's first numbered lines are declarations (`10 Dim a`), the first
/// numbered line with executable code reports the first numbered line
/// (10), unless it is the last numbered line of the procedure.
let private erlLines (body: Statement list) =
    let labels = ResizeArray<string * bool>()
    let mutable pending : string option = None
    let isNumeric (label: string) = label.Length > 0 && label.Length < 10 && Seq.forall System.Char.IsDigit label
    let rec visit (stmt: Statement) =
        match stmt with
        | LocatedStmt (_, inner) -> visit inner
        | LabelStmt label when isNumeric label ->
            match pending with
            | Some previous -> labels.Add((previous, false))
            | None -> ()
            pending <- Some label
        | LabelStmt _ -> ()
        | other ->
            match pending with
            | Some label ->
                let executable =
                    match other with
                    | Declaration (_, (Dim _ | Const _ | StaticDim _)) -> false
                    | _ -> true
                labels.Add((label, executable))
                pending <- None
            | None -> ()
            match other with
            | IfStmt (_, yes, alternatives, no) ->
                for s in yes do visit s
                for (_, stmts) in alternatives do for s in stmts do visit s
                match no with Some stmts -> (for s in stmts do visit s) | None -> ()
            | ForLoop (_, _, _, _, stmts) | ForEach (_, _, stmts) | WhileLoop (_, stmts) | WithStmt (_, stmts) ->
                for s in stmts do visit s
            | SelectCase (_, cases) -> for (_, stmts) in cases do for s in stmts do visit s
            | DoLoop (a, b) ->
                for c in [ a; b ] do
                    match c with Some c -> (for s in c.Body do visit s) | None -> ()
            | _ -> ()
    for stmt in body do visit stmt
    match pending with Some label -> labels.Add((label, false)) | None -> ()
    let map = Dictionary<string, int>()
    match Seq.tryFindIndex snd labels with
    | Some i when i > 0 && i < labels.Count - 1 -> map.[fst labels.[i]] <- int (fst labels.[0])
    | _ -> ()
    map

let mutable private visitStatic : VarDeclarator -> unit = ignore

let rec private collectDeclarations (stmt: Statement) (visit: VarDeclarator -> unit) (visitConst: string * TypeRef option * Expression -> unit) =
    match stmt with
    | LocatedStmt (_, inner) -> collectDeclarations inner visit visitConst
    | Declaration (_, Dim ds) -> for d in ds do visit d
    | Declaration (_, StaticDim ds) -> for d in ds do visitStatic d
    | Declaration (_, Const (n, t, v)) -> visitConst (n, t, v)
    | IfStmt (_, yes, alternatives, no) ->
        for stmt in yes do collectDeclarations stmt visit visitConst
        for (_, stmts) in alternatives do
            for stmt in stmts do collectDeclarations stmt visit visitConst
        match no with
        | Some stmts -> for stmt in stmts do collectDeclarations stmt visit visitConst
        | None -> ()
    | ForLoop (_, _, _, _, stmts) | ForEach (_, _, stmts) | WhileLoop (_, stmts) | WithStmt (_, stmts) ->
        for stmt in stmts do collectDeclarations stmt visit visitConst
    | SelectCase (_, cases) ->
        for (_, stmts) in cases do
            for stmt in stmts do collectDeclarations stmt visit visitConst
    | DoLoop (a, b) ->
        for condition in [a; b] do
            match condition with
            | Some c -> for stmt in c.Body do collectDeclarations stmt visit visitConst
            | None -> ()
    | _ -> ()

let compileFunctionBodyWith (ctx: CompileContext) (isStatic: bool) (name: string) (returnName: string) (paramList: Parameter list) (returnType: TypeRef option) (body: Statement list) =
    // A local declared with the procedure's own name (`Sub color()` /
    // `Dim color`) is a variable that shadows the procedure; the return value
    // then lives in a hidden slot.
    let mutable shadowed = false
    let declares (n: string) = if normalizeName n = normalizeName returnName then shadowed <- true
    let savedVisitStatic = visitStatic
    visitStatic <- (fun d -> declares d.Name)
    for stmt in body do
        collectDeclarations stmt (fun d -> declares d.Name) (fun (n, _, _) -> declares n)
    visitStatic <- savedVisitStatic
    let returnName = if shadowed then returnName + "#return" else returnName
    let outerErlLines = ctx.ErlLines
    ctx.ErlLines <- erlLines body
    let outerLocals = copyNameDictionary ctx.Locals
    let outerStatics = ctx.StaticLocals
    let outerAsNew = ctx.LocalAsNew
    ctx.StaticLocals <- newNameDictionary ()
    ctx.LocalAsNew <- newNameDictionary ()
    let outerNextLocal = ctx.NextLocalSlot
    let outerReturnSlot = ctx.ReturnSlotName
    let outerClassFields = ctx.ClassFields
    let outerLabels = Dictionary(ctx.Labels)
    let outerPatches = ResizeArray(ctx.LabelPatches)
    let outerTypes = ctx.LocalTypes
    let outerWith = ctx.WithTargetSlot

    ctx.Locals <- newNameDictionary ()
    ctx.LocalTypes <- newNameDictionary ()
    ctx.NextLocalSlot <- 0
    ctx.ReturnSlotName <- Some returnName
    ctx.InFunction <- true
    ctx.WithTargetSlot <- None
    ctx.Labels.Clear()
    ctx.LabelPatches.Clear()

    // Reserve slot 0 for Me if inside a class method
    if ctx.ClassFields.IsSome then
        ignore (getLocalSlot ctx "__me__")

    for param in paramList do
        ignore (getLocalSlot ctx param.Name)
        match param.Type with
        | Some t when not param.IsArray && not param.IsParamArray -> setName ctx.LocalTypes param.Name t.Name
        | _ -> ()
        if param.IsArray || param.IsParamArray then addSetName ctx.ArrayVars param.Name |> ignore

    ignore (getLocalSlot ctx returnName)
    match returnType with
    | Some t -> setName ctx.LocalTypes returnName t.Name
    | None -> ()
    let declared = ResizeArray<VarDeclarator>()
    // Static locals live in hidden module variables named "Proc#name".
    let declareStatic (d: VarDeclarator) =
        let key = name + "#" + d.Name
        if not (containsName ctx.Globals key) then
            ignore (getGlobalSlot ctx key)
            match d.Type with
            | Some t -> setName ctx.GlobalTypes key t.Name
            | None -> ()
            if d.IsNew then (match d.Type with Some t -> setName ctx.AsNew key t.Name | None -> ())
            ctx.StaticInits.Add((ctx.ModuleName, key, d))
        setName ctx.StaticLocals d.Name key
        match d.ArraySpec with
        | Some spec ->
            addSetName ctx.ArrayVars d.Name |> ignore
            if not spec.Dimensions.IsEmpty then addSetName ctx.FixedArrays d.Name |> ignore
        | None -> ()
    let outerVisit = visitStatic
    visitStatic <- declareStatic
    for stmt in body do
        collectDeclarations stmt (fun d ->
            if isStatic then declareStatic d
            else
            ignore (getLocalSlot ctx d.Name)
            declared.Add d
            if d.IsNew then (match d.Type with Some t -> setName ctx.LocalAsNew d.Name t.Name | None -> ())
            match d.ArraySpec with
            | Some spec when not spec.Dimensions.IsEmpty -> addSetName ctx.FixedArrays d.Name |> ignore
            | _ -> ()
            match d.Type, d.ArraySpec with
            | Some t, _ -> setName ctx.LocalTypes d.Name t.Name
            | _ -> ()) (fun (n, t, v) ->
                ignore (getLocalSlot ctx n)
                match constTypeName t v with
                | Some typeName -> setName ctx.LocalTypes n typeName
                | None -> ())
    visitStatic <- outerVisit

    let funcIndex = getName ctx.FunctionIndex name
    let startPc = ctx.Instructions.Count

    // Prologue: typed locals and return value start at their type's default;
    // ByVal typed parameters convert the passed value.
    let line = ctx.CurrentLine
    // A ByRef typed parameter given a value rather than a variable (an
    // expression, an Optional default, a host argument) converts it too.
    let coercions = ResizeArray<int>()
    for param in paramList do
        match param.Type with
        | Some t when not param.IsArray && not param.IsParamArray ->
            match ValueSemantics.declaredTypeOf t.Name with
            | Some dt -> coercions.Add(getName ctx.Locals param.Name); coercions.Add(int dt)
            | None -> ()
        | _ -> ()
    if coercions.Count > 0 then emit ctx (CoerceParams (coercions.ToArray())) line
    // Scalar defaults go into the frame template; user-defined types are
    // constructed by the prologue.
    let initial = Dictionary<int, Value>()
    let initialise slot (typeName: string) =
        match ValueSemantics.declaredTypeOf typeName with
        | Some dt -> initial.[slot] <- ValueSemantics.defaultOf dt
        | None ->
            match udtIndex ctx typeName with
            | Some idx ->
                emit ctx (NewObj idx) line
                emit ctx (StoreLocal slot) line
            | None -> ()
    match returnType with
    | Some t when not (normalizeName returnName = "class_initialize") -> initialise (getName ctx.Locals returnName) t.Name
    | _ -> ()
    for d in declared do
        match d.Type, d.ArraySpec with
        | Some _, None when d.IsNew -> initial.[getName ctx.Locals d.Name] <- VNothing
        | Some t, None -> initialise (getName ctx.Locals d.Name) t.Name
        | _, Some spec when spec.Dimensions.IsEmpty ->
            emit ctx (CallBuiltin ("array:unallocated", 0)) line
            emit ctx (StoreLocal (getName ctx.Locals d.Name)) line
        | _ -> ()

    for stmt in body do
        compileStatement ctx stmt

    let retSlot = getName ctx.Locals returnName
    emit ctx (LoadLocal retSlot) 0
    emit ctx Return 0

    patchLabels ctx

    let localCount = ctx.NextLocalSlot

    ctx.Locals <- outerLocals
    ctx.StaticLocals <- outerStatics
    ctx.LocalAsNew <- outerAsNew
    ctx.LocalTypes <- outerTypes
    ctx.NextLocalSlot <- outerNextLocal
    ctx.ReturnSlotName <- outerReturnSlot
    ctx.ErlLines <- outerErlLines
    ctx.InFunction <- false
    ctx.ClassFields <- outerClassFields
    ctx.WithTargetSlot <- outerWith
    ctx.Labels.Clear()
    for kv in outerLabels do setName ctx.Labels kv.Key kv.Value
    ctx.LabelPatches.Clear()
    for p in outerPatches do ctx.LabelPatches.Add(p)

    let byRefArr = paramList |> List.map (fun p -> p.ByRef) |> List.toArray
    ctx.Functions.[funcIndex] <- {
        Name = name
        ParamsCount = List.length paramList
        LocalsCount = localCount
        ByRefParams = byRefArr
        Code = [||]
        StartPC = startPc
        Module = ctx.ModuleName
        ParamNames = paramList |> List.map (fun p -> p.Name) |> List.toArray
        InitialLocals =
            if initial.Count = 0 then [||]
            else
                let template = Array.create (max localCount 1) VEmpty
                for kv in initial do template.[kv.Key] <- kv.Value
                template
    }

let compileFunctionBody (ctx: CompileContext) (name: string) (returnName: string) (paramList: Parameter list) (returnType: TypeRef option) (body: Statement list) =
    compileFunctionBodyWith ctx false name returnName paramList returnType body

let private registerFunction (ctx: CompileContext) name (parameters: Parameter list) =
    if containsName ctx.FunctionIndex name then fail ctx (sprintf "Ambiguous name detected: %s" name)
    let modes = parameters |> List.map (fun p -> p.ByRef) |> List.toArray
    let index = ctx.Functions.Count
    setName ctx.FunctionIndex name index
    setName ctx.Parameters name parameters
    ctx.Functions.Add({ Name = name; ParamsCount = modes.Length; LocalsCount = 0
                        ByRefParams = modes; Code = [||]; StartPC = 0; Module = ctx.ModuleName
                        ParamNames = parameters |> List.map (fun p -> p.Name) |> List.toArray; InitialLocals = [||] })
    index

let private isPrivate (mods: Modifier list) = mods |> List.contains (VisibilityMod Private)

let private functionModifiers func =
    match func with
    | FunctionDecl (m, _, _, _, _) | SubDecl (m, _, _, _) | PropertyGet (m, _, _, _, _)
    | PropertyLet (m, _, _, _) | PropertySet (m, _, _, _) -> m

/// Register the procedures and classes of one module. In a named module a
/// procedure is registered as `Module.Proc`, and public procedures of
/// standard modules also under their bare name.
let private registerDeclarations (ctx: CompileContext) (kind: ModuleKind) (tops: TopLevel list) =
    let register prefix func =
        let suffix, ps =
            match func with
            | FunctionDecl (_, name, ps, _, _) | SubDecl (_, name, ps, _) -> name, ps
            | PropertyGet (_, name, ps, _, _) -> name + "_get", ps
            | PropertyLet (_, name, ps, _) -> name + "_let", ps
            | PropertySet (_, name, ps, _) -> name + "_set", ps
        let returnType =
            match func with
            | FunctionDecl (_, _, _, Some t, _) | PropertyGet (_, _, _, Some t, _) -> Some t.Name
            | _ -> None
        let index =
            if prefix = "" && ctx.ModuleName <> "" then
                let key = privateKey ctx.ModuleName suffix
                let index = registerFunction ctx key ps
                if kind = StandardModule && not (isPrivate (functionModifiers func)) && not (containsName ctx.FunctionIndex suffix) then
                    setName ctx.FunctionIndex suffix index
                    setName ctx.Parameters suffix ps
                index
            else registerFunction ctx (prefix + suffix) ps
        match returnType with
        | Some t -> ctx.FunctionTypes.[index] <- t
        | None -> ()
    for top in tops do
        match top with
        | FunctionDef f -> register "" f
        | ClassDecl (_, name, members) ->
            if containsName ctx.ClassIndex name then fail ctx (sprintf "Ambiguous name detected: %s" name)
            setName ctx.ClassIndex name ctx.Classes.Count
            ctx.Classes.Add({ Name = name; Fields = [||]; Methods = Map.empty; PropertyGetters = Map.empty; PropertyLetters = Map.empty
                              PropertySetters = Map.empty; IsValueType = false; FieldTypes = [||]; DefaultMember = ""; Implements = [||] })
            for memberDecl in members do
                match memberDecl with FunctionDef f -> register (name + ".") f | _ -> ()
            let events = members |> List.choose (function EventDecl (_, eventName, _) -> Some eventName | _ -> None)
            if not events.IsEmpty then setName ctx.ClassEvents name events
        | TypeDecl (_, name, _) ->
            if containsName ctx.ClassIndex name then fail ctx (sprintf "Ambiguous name detected: %s" name)
            setName ctx.ClassIndex name ctx.Classes.Count
            ctx.Classes.Add({ Name = name; Fields = [||]; Methods = Map.empty; PropertyGetters = Map.empty; PropertyLetters = Map.empty
                              PropertySetters = Map.empty; IsValueType = true; FieldTypes = [||]; DefaultMember = ""; Implements = [||] })
        | _ -> ()

let private fieldTypeCode (ctx: CompileContext) (typeName: string option) =
    match typeName with
    | None -> 0
    | Some t ->
        match ValueSemantics.declaredTypeOf t with
        | Some dt -> int dt
        | None ->
            match udtIndex ctx t with
            | Some idx -> -(idx + 1)
            | None -> 0

/// Fill in the field lists of user-defined types (after all are registered).
let private defineTypes (ctx: CompileContext) (tops: TopLevel list) =
    for top in tops do
        match top with
        | TypeDecl (_, name, members) ->
            let idx = getName ctx.ClassIndex name
            ctx.Classes.[idx] <-
                { ctx.Classes.[idx] with
                    Fields = members |> List.map (fun m -> m.Name) |> List.toArray
                    FieldTypes = members |> List.map (fun m -> fieldTypeCode ctx (Some m.Type.Name)) |> List.toArray }
        | _ -> ()

let rec compileFunctions (ctx: CompileContext) (topLevel: TopLevel) =
    let key (name: string) = if ctx.ModuleName = "" then name else privateKey ctx.ModuleName name
    match topLevel with
    | FunctionDef func ->
        match func with
        | FunctionDecl (mods, name, paramList, returnType, body) -> compileFunctionBodyWith ctx (List.contains StaticMod mods) (key name) name paramList returnType body
        | SubDecl (mods, name, paramList, body) -> compileFunctionBodyWith ctx (List.contains StaticMod mods) (key name) name paramList None body
        | PropertyGet (mods, name, paramList, returnType, body) -> compileFunctionBodyWith ctx (List.contains StaticMod mods) (key (name + "_get")) name paramList returnType body
        | PropertyLet (mods, name, paramList, body) -> compileFunctionBodyWith ctx (List.contains StaticMod mods) (key (name + "_let")) name paramList None body
        | PropertySet (mods, name, paramList, body) -> compileFunctionBodyWith ctx (List.contains StaticMod mods) (key (name + "_set")) name paramList None body
    | ClassDecl (_, className, members) ->
        let outerClassFields = ctx.ClassFields
        let outerClassName = ctx.ClassName
        ctx.ClassName <- Some className
        let mutable fields = []
        let mutable methods = Map.empty
        let mutable getters = Map.empty
        let mutable letters = Map.empty
        let mutable setters = Map.empty

        // First pass: collect field names
        for m in members do
            match m with
            | TopLevelStatement (LocatedStmt (_, Declaration (_, Dim declarators)))
            | TopLevelStatement (Declaration (_, Dim declarators)) ->
                for d in declarators do
                    let fieldType =
                        match d.ArraySpec with
                        | Some _ -> None
                        | None -> if d.IsNew then Some "Object" else d.Type |> Option.map (fun t -> t.Name)
                    fields <- (d.Name, fieldType) :: fields
                    if d.IsNew then (match d.Type with Some t -> setName ctx.AsNew (className + "." + d.Name) t.Name | None -> ())
            | WithEventsDecl (_, name, _) -> fields <- (name, Some "Object") :: fields
            | _ -> ()

        let fieldSet = newNameSet ()
        for (field, _) in List.rev fields do
            addSetName fieldSet field |> ignore

        for m in members do
            match m with
            | FunctionDef func ->
                ctx.ClassFields <- Some fieldSet
                let isStatic = List.contains StaticMod (functionModifiers func)
                let compileMember (name: string) suffix paramList returnType body =
                    let fullName = className + "." + name + suffix
                    compileFunctionBodyWith ctx isStatic fullName name paramList returnType body
                    getName ctx.FunctionIndex fullName
                match func with
                | SubDecl (_, name, paramList, body) ->
                    methods <- Map.add (normalizeName name) (compileMember name "" paramList None body) methods
                | FunctionDecl (_, name, paramList, returnType, body) ->
                    methods <- Map.add (normalizeName name) (compileMember name "" paramList returnType body) methods
                | PropertyGet (_, name, paramList, returnType, body) ->
                    getters <- Map.add (normalizeName name) (compileMember name "_get" paramList returnType body) getters
                | PropertyLet (_, name, paramList, body) ->
                    letters <- Map.add (normalizeName name) (compileMember name "_let" paramList None body) letters
                | PropertySet (_, name, paramList, body) ->
                    setters <- Map.add (normalizeName name) (compileMember name "_set" paramList None body) setters
            | _ -> ()

        let classIdx = getName ctx.ClassIndex className
        let orderedFields = List.rev fields
        // `Implements I`: members named I_X answer calls to X (unless the class has its own X).
        let interfaces = members |> List.choose (function ImplementsDecl i -> Some i | _ -> None)
        let alias (m: Map<string, int>) =
            let mutable result = m
            for i in interfaces do
                let prefix = normalizeName i + "_"
                for KeyValue (key, index) in m do
                    if key.StartsWith prefix then
                        let plain = key.Substring prefix.Length
                        if not (Map.containsKey plain result) && not (containsSetName fieldSet plain) then
                            result <- Map.add plain index result
            result
        let methods, getters, letters, setters = alias methods, alias getters, alias letters, alias setters
        let defaultMember =
            match tryGetName ctx.DefaultMembers className with
            | true, name -> normalizeName name
            | _ ->
                members |> List.tryPick (function
                    | FunctionDef f when List.contains DefaultMod (functionModifiers f) ->
                        match f with
                        | PropertyGet (_, name, _, _, _) | FunctionDecl (_, name, _, _, _) | SubDecl (_, name, _, _)
                        | PropertyLet (_, name, _, _) | PropertySet (_, name, _, _) -> Some (normalizeName name)
                    | _ -> None)
                |> Option.defaultValue ""
        ctx.Classes.[classIdx] <- {
            Name = className
            Fields = orderedFields |> List.map fst |> List.toArray
            Methods = methods
            PropertyGetters = getters
            PropertyLetters = letters
            PropertySetters = setters
            IsValueType = false
            FieldTypes = orderedFields |> List.map (fun (_, t) -> fieldTypeCode ctx t) |> List.toArray
            DefaultMember = defaultMember
            Implements = interfaces |> List.toArray
        }
        ctx.ClassFields <- outerClassFields
        ctx.ClassName <- outerClassName
    | _ -> ()

let rec compileTopLevel (ctx: CompileContext) (topLevel: TopLevel) =
    match topLevel with
    | TopLevelStatement stmt -> compileStatement ctx stmt
    | EnumDecl (_, _, members) ->
        // Members are module-level constants; unvalued members count up.
        let mutable previous = None
        for m in members do
            match m.Value, previous with
            | Some value, _ -> compileExpression ctx value
            | None, None -> emitConst ctx (VInteger 0)
            | None, Some prev ->
                emitLoadVar ctx prev
                emitConst ctx (VInteger 1)
                emit ctx AddChecked 0
            storeName ctx m.Name
            previous <- Some m.Name
    | OptionStmt option when normalizeName option = "compare text" -> ctx.TextCompare <- true
    | _ -> ()

let private declarationVisibilityIsPrivate (vis: Visibility option) =
    match vis with
    | Some Public | Some Global -> false
    | _ -> true

let rec preRegisterGlobals (ctx: CompileContext) (topLevel: TopLevel) =
    let register (vis: Visibility option) name (typeName: string option) =
        let key =
            if ctx.ModuleName <> "" && declarationVisibilityIsPrivate vis then privateKey ctx.ModuleName name
            else name
        ignore (getGlobalSlot ctx key)
        match typeName with
        | Some t -> setName ctx.GlobalTypes key t
        | None -> ()
    match topLevel with
    | TopLevelStatement (LocatedStmt (_, stmt)) -> preRegisterGlobals ctx (TopLevelStatement stmt)
    | TopLevelStatement (Declaration (vis, (Dim declarators | StaticDim declarators))) ->
        for d in declarators do
            register vis d.Name (if d.IsNew then None else d.Type |> Option.map (fun t -> t.Name))
            if d.IsNew then
                let key = if ctx.ModuleName <> "" && declarationVisibilityIsPrivate vis then privateKey ctx.ModuleName d.Name else d.Name
                match d.Type with Some t -> setName ctx.AsNew key t.Name | None -> ()
            match d.ArraySpec with
            | Some spec ->
                addSetName ctx.ArrayVars d.Name |> ignore
                if not spec.Dimensions.IsEmpty then addSetName ctx.FixedArrays d.Name |> ignore
            | None -> ()
    | WithEventsDecl (vis, name, _) -> register vis name None
    | TopLevelStatement (Declaration (vis, Const (name, typeRef, value))) -> register vis name (constTypeName typeRef value)
    | TopLevelStatement (Declaration (_, ReDim (name, _, _))) ->
        if (tryResolveGlobal ctx name).IsNone then register None name None
    | EnumDecl (vis, enumName, members) ->
        let names = newNameSet ()
        for m in members do
            register (match vis with None -> Some Public | v -> v) m.Name (Some "Long")
            addSetName names m.Name |> ignore
        setName ctx.Enums enumName names
    | _ -> ()

let private globalNames (ctx: CompileContext) =
    let names = Array.create ctx.NextGlobalSlot ""
    for kvp in ctx.Globals do
        names.[kvp.Value] <- kvp.Key
    names

let toProgram (ctx: CompileContext) : BytecodeProgram =
    {
        TextCompare = ctx.TextCompare
        Code = ctx.Instructions.ToArray()
        Constants = ctx.Constants.ToArray()
        Globals = ctx.NextGlobalSlot
        GlobalNames = globalNames ctx
        Functions = ctx.Functions.ToArray()
        Classes = ctx.Classes.ToArray()
        ModuleStarts = ctx.ModuleStarts.ToArray()
        EventBindings = ctx.EventBindings.ToArray()
    }

/// A module of a project: its name, kind and parsed program.
type CompileModule = {
    Name: string
    Kind: ModuleKind
    Program: Program
    /// A class module's default member (`Attribute X.VB_UserMemId = 0`).
    DefaultMember: string option
}

let private withModule (ctx: CompileContext) (m: CompileModule) action =
    ctx.ModuleName <- m.Name
    ctx.CurrentLine <- 0
    ctx.OptionBase <-
        if m.Program.TopLevels |> List.exists (function OptionStmt o -> normalizeName o = "base 1" | _ -> false) then 1 else 0
    ctx.OptionExplicit <- m.Program.TopLevels |> List.exists (function OptionStmt o -> normalizeName o = "explicit" | _ -> false)
    action ()
    ctx.ModuleName <- ""
    ctx.OptionBase <- 0
    ctx.OptionExplicit <- false

let private moduleTopLevels (m: CompileModule) =
    match m.Kind with
    | ClassModule ->
        // A class module's body is one class named after the module.
        let classMembers, rest =
            m.Program.TopLevels |> List.partition (function
                | FunctionDef _ | TopLevelStatement _ | EventDecl _ | WithEventsDecl _ | ImplementsDecl _ -> true
                | _ -> false)
        rest @ [ ClassDecl (Some Public, m.Name, classMembers) ]
    | _ -> m.Program.TopLevels

/// Fixed-size array fields (`Private items(10)`) are created by the class's
/// Class_Initialize, ahead of its own statements.
let private withFieldInitializers (members: TopLevel list) =
    let arrayFields =
        members |> List.collect (function
            | TopLevelStatement (LocatedStmt (_, Declaration (_, Dim ds)))
            | TopLevelStatement (Declaration (_, Dim ds)) ->
                ds |> List.choose (fun d ->
                    match d.ArraySpec with
                    | Some spec when not spec.Dimensions.IsEmpty -> Some (Declaration (None, ReDim (d.Name, spec.Dimensions, false)))
                    | _ -> None)
            | _ -> [])
    if arrayFields.IsEmpty then members
    else
        let isInitialize f = match f with SubDecl (_, name, [], _) -> normalizeName name = "class_initialize" | _ -> false
        if members |> List.exists (function FunctionDef f -> isInitialize f | _ -> false) then
            members |> List.map (function
                | FunctionDef (SubDecl (mods, name, ps, body)) when normalizeName name = "class_initialize" ->
                    FunctionDef (SubDecl (mods, name, ps, arrayFields @ body))
                | other -> other)
        else members @ [ FunctionDef (SubDecl ([ VisibilityMod Private ], "Class_Initialize", [], arrayFields)) ]

let rec private prepareTopLevels (tls: TopLevel list) =
    tls |> List.map (function
        | ClassDecl (vis, name, members) -> ClassDecl (vis, name, withFieldInitializers members)
        | other -> other)

/// Compile several modules into one program. Module-level code runs in module
/// order; `ModuleStarts` records where each module's code begins.
let compileModules (hostNames: string seq) (modules: CompileModule list) : BytecodeProgram * CompileContext =
    let ctx = emptyContext ()
    for name in hostNames do
        if not (System.String.IsNullOrWhiteSpace name) then
            ignore (getGlobalSlot ctx name)
    let named = modules |> List.filter (fun m -> m.Name <> "")
    for m in named do
        if m.Kind <> ClassModule then addSetName ctx.ModuleNames m.Name |> ignore
    let tops = modules |> List.map (fun m -> m, prepareTopLevels (moduleTopLevels m))
    for m, tls in tops do
        withModule ctx m (fun () -> for tl in tls do preRegisterGlobals ctx tl)
    for m in modules do
        match m.Kind, m.DefaultMember with
        | ClassModule, Some memberName -> setName ctx.DefaultMembers m.Name memberName
        | _ -> ()
    for m, tls in tops do
        withModule ctx m (fun () -> registerDeclarations ctx m.Kind tls)
    for m, tls in tops do
        withModule ctx m (fun () -> defineTypes ctx tls)
    ctx.HasTerminate <- ctx.FunctionIndex.Keys |> Seq.exists (fun k -> normalizeName(k).EndsWith ".class_terminate")
    // WithEvents variables: their handlers are the procedures Variable_Event.
    let rec bindWithEvents (scope: string) (isClass: bool) (tls: TopLevel list) =
        for tl in tls do
            match tl with
            | WithEventsDecl (_, variable, typeRef) ->
                let events = match tryGetName ctx.ClassEvents typeRef.Name with | true, e -> e | _ -> []
                let handlers =
                    events |> List.choose (fun eventName ->
                        let procName = variable + "_" + eventName
                        let key = if scope = "" then procName else scope + "." + procName
                        match tryGetName ctx.FunctionIndex key with
                        | true, index -> Some (normalizeName eventName, index)
                        | _ -> None)
                setName ctx.WithEventsVars (scope + "." + variable) ctx.EventBindings.Count
                ctx.EventBindings.Add { Handlers = Map.ofList handlers; ClassSink = isClass }
            | ClassDecl (_, className, members) -> bindWithEvents className true members
            | _ -> ()
    for m, tls in tops do bindWithEvents m.Name false tls
    emit ctx (Jump -1) 0
    for m, tls in tops do
        withModule ctx m (fun () -> for tl in tls do compileFunctions ctx tl)
    patchJump ctx 0 (Jump ctx.Instructions.Count)
    // Static locals start at their type's default, once.
    for moduleName, key, d in ctx.StaticInits do
        let m = modules |> List.tryFind (fun m -> m.Name = moduleName)
        let init () =
            let slot = getGlobalSlot ctx key
            let typeName = d.Type |> Option.map (fun t -> t.Name)
            match d.ArraySpec with
            | Some spec when not spec.Dimensions.IsEmpty ->
                emitDimensions ctx spec.Dimensions
                emit ctx (ArrayNew spec.Dimensions.Length) 0
                emitArrayFill ctx typeName
                emit ctx (StoreGlobal slot) 0
            | Some _ ->
                emit ctx (CallBuiltin ("array:unallocated", 0)) 0
                emit ctx (StoreGlobal slot) 0
            | None ->
                if d.IsNew then
                    emitConst ctx VNothing
                    emit ctx (StoreGlobal slot) 0
                else
                    match typeName with
                    | Some t -> if emitTypedInitial ctx t then emit ctx (StoreGlobal slot) 0
                    | None -> ()
        match m with
        | Some m -> withModule ctx m init
        | None -> init ()
    for m, tls in tops do
        withModule ctx m (fun () ->
            ctx.ModuleStarts.Add((ctx.Instructions.Count, m.Name))
            ctx.Labels.Clear()
            ctx.LabelPatches.Clear()
            // Enums first, so procedures and later code see their values.
            for tl in tls do (match tl with EnumDecl _ | OptionStmt _ -> compileTopLevel ctx tl | _ -> ())
            for tl in tls do (match tl with EnumDecl _ | OptionStmt _ -> () | _ -> compileTopLevel ctx tl)
            patchLabels ctx)
    emit ctx Halt 0
    toProgram ctx, ctx

let compileWithGlobalNames (names: string seq) (program: Program) : BytecodeProgram =
    compileModules names [ { Name = ""; Kind = StandardModule; Program = program; DefaultMember = None } ] |> fst

let compile (program: Program) : BytecodeProgram =
    compileWithGlobalNames Seq.empty program

/// Compile host-entered code (an Immediate-window statement or expression)
/// against an existing context. The code runs in its own frame: it starts at
/// the returned pc and ends with `Return`, leaving its value on the stack.
/// Returns (start pc, locals count).
let compileSnippet (ctx: CompileContext) (moduleName: string) (body: Choice<Expression, Statement list>) =
    let outerLocals = ctx.Locals
    let outerNext = ctx.NextLocalSlot
    ctx.Locals <- newNameDictionary ()
    ctx.LocalTypes <- newNameDictionary ()
    ctx.NextLocalSlot <- 0
    ctx.InFunction <- false
    ctx.ReturnSlotName <- None
    ctx.WithTargetSlot <- None
    ctx.ModuleName <- moduleName
    ctx.Labels.Clear()
    ctx.LabelPatches.Clear()
    let start = ctx.Instructions.Count
    try
        try
            ctx.OptionExplicit <- false
            match body with
            | Choice1Of2 expr ->
                compileExpression ctx expr
            | Choice2Of2 stmts ->
                for stmt in stmts do compileStatement ctx stmt
                emit ctx LoadEmpty 0
            emit ctx Return 0
            patchLabels ctx
            start, max 1 ctx.NextLocalSlot
        with _ ->
            ctx.Instructions.RemoveRange(start, ctx.Instructions.Count - start)
            reraise ()
    finally
        ctx.Locals <- outerLocals
        ctx.NextLocalSlot <- outerNext
        ctx.ModuleName <- ""
