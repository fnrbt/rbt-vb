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

let private resolveVar (ctx: CompileContext) (name: string) (forWrite: bool) =
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
                    // Undeclared names in module-level code are module variables,
                    // so they survive into later host calls and snippets.
                    if not ctx.InFunction && ctx.ClassFields.IsNone then GlobalLoc (getGlobalSlot ctx name)
                    else LocalLoc (getLocalSlot ctx name)

/// The declared type name of a variable, if any.
let private declaredTypeName (ctx: CompileContext) name =
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

let private emitLoadVar (ctx: CompileContext) name =
    match resolveVar ctx name false with
    | FieldLoc ->
        emit ctx (LoadLocal 0) 0
        emit ctx (GetMember name) 0
    | GlobalLoc slot -> emit ctx (LoadGlobal slot) 0
    | LocalLoc slot -> emit ctx (LoadLocal slot) 0
    | ConstLoc value -> emitConst ctx value

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
    isField ctx name || containsName ctx.Locals name || (tryResolveGlobal ctx name).IsSome

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
        | "createobject" | "getobject" -> true
        | _ -> false
    | _ -> true

/// The declared type a literal or typed variable already has, if static.
let private staticType (ctx: CompileContext) expr =
    match expr with
    | Literal (Integer i) when i >= -32768 && i <= 32767 -> Some [ ValueSemantics.DeclaredType.Integer; ValueSemantics.DeclaredType.Long ]
    | Literal (Integer _) -> Some [ ValueSemantics.DeclaredType.Long ]
    | Literal (Double _) -> Some [ ValueSemantics.DeclaredType.Double ]
    | Literal (String _) -> Some [ ValueSemantics.DeclaredType.String ]
    | Literal (Boolean _) -> Some [ ValueSemantics.DeclaredType.Boolean ]
    | Identifier name ->
        declaredTypeName ctx name |> Option.bind ValueSemantics.declaredTypeOf |> Option.map List.singleton
    | _ -> None

/// Push one argument for a by-reference parameter: a reference when the
/// argument is a variable, array element or field, else its value.
let rec private emitRefArgument (ctx: CompileContext) (arg: Expression) (compileExpr: CompileContext -> Expression -> unit) =
    match arg with
    | Identifier varName when isField ctx varName ->
        emit ctx (LoadLocal 0) 0
        emit ctx (MakeRefMember varName) 0
    | Identifier varName when isVariableName ctx varName && not ((tryResolveFunction ctx varName).IsSome && not (isLocalVariable ctx varName) && (tryResolveGlobal ctx varName).IsNone) ->
        match resolveVar ctx varName true with
        | GlobalLoc slot -> emit ctx (MakeRefGlobal slot) 0
        | LocalLoc slot -> emit ctx (MakeRefLocal slot) 0
        | _ -> compileExpr ctx arg
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

let rec compileExpression (ctx: CompileContext) (expr: Expression) =
    match expr with
    | Parenthesized e -> compileExpression ctx e
    | ArrayBound _ -> fail ctx "Array bound outside declaration"
    | MissingArg -> emitConst ctx VUndefined
    | NamedArg (name, _) -> fail ctx (sprintf "Named argument not expected here: %s" name)
    | Literal (Integer i) -> emitConst ctx (VInteger i)
    | Literal (Double d) -> emitConst ctx (VDouble d)
    | Literal (String s) -> emitConst ctx (VString s)
    | Literal (Boolean b) -> if b then emit ctx LoadTrue 0 else emit ctx LoadFalse 0
    | Literal Null -> emit ctx LoadNull 0
    | Literal Empty -> emit ctx LoadEmpty 0
    | Literal Nothing -> emitConst ctx VNothing
    | Identifier name when isErrName ctx name -> emit ctx LoadErrObject 0
    | Identifier name when not (isVariableName ctx name) && (tryResolveFunction ctx name).IsSome ->
        // A parameterless function used as a value.
        emitCall ctx name [] compileExpression
    | Identifier name -> emitLoadVar ctx name
    | Unary (Ast.Not, e) ->
        compileExpression ctx e
        emit ctx Bytecode.Not 0
    | Unary (Ast.Negate, e) ->
        compileExpression ctx e
        emit ctx Bytecode.Negate 0
    | Binary (op, left, right) ->
        compileExpression ctx left
        compileExpression ctx right
        emit ctx (binaryOpcode op) 0
    | Ast.Call (Identifier name, args) when
        (match ctx.ClassName with Some className -> containsName ctx.FunctionIndex (className + "." + name) | None -> false) ->
        compileExpression ctx (Ast.Call (Member (MeExpr, name), args))
    | Ast.Call (Identifier name, args) when isField ctx name ->
        emit ctx (LoadLocal 0) 0
        emit ctx (GetMember name) 0
        emitDefaultCall ctx args compileExpression
    | Ast.Call (Identifier name, args) ->
        let isKnownValue = containsSetName ctx.ArrayVars name || isVariableName ctx name
        let isFunction = (tryResolveFunction ctx name).IsSome
        // Inside Function F, F(x) is a recursive call unless F is an array.
        if isKnownValue && (not isFunction || (containsSetName ctx.ArrayVars name && isLocalVariable ctx name && ctx.ReturnSlotName <> Some name)) then
            emitLoadVar ctx name
            if containsSetName ctx.ArrayVars name && not args.IsEmpty && not (hasNamed args) && not (List.contains MissingArg args) then
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
        | "print" ->
            emitArguments ctx args compileExpression
            emit ctx (CallBuiltin ("debug.print", List.length args)) 0
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
    | NewExpr className ->
        match tryGetName ctx.ClassIndex className with
        | true, idx -> emit ctx (NewObj idx) 0
        | false, _ -> fail ctx (sprintf "User-defined type not defined: %s" className)
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
    match target |> Option.bind ValueSemantics.declaredTypeOf, staticType ctx expr with
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

    | Assignment (name, expr) | Let (name, expr) ->
        match ctx.ReturnSlotName with
        | Some returnName when normalizeName returnName = normalizeName name && ctx.InFunction ->
            compileTypedValue ctx expr name
        | _ ->
        match resolveVar ctx name true with
        | FieldLoc ->
            emit ctx (LoadLocal 0) 0
            compileLetValue ctx expr false
            emit ctx (SetMember (name, false)) 0
        | _ -> compileTypedValue ctx expr name

    | IndexedAssignment (name, indices, valueExpr) ->
        emitLoadVar ctx name
        for index in indices do compileExpression ctx index
        let elementType = declaredTypeName ctx name
        let converts = match elementType with Some t -> (ValueSemantics.declaredTypeOf t).IsSome | None -> false
        compileLetValue ctx valueExpr converts
        emitStoreConversion ctx elementType
        emit ctx (ArraySet indices.Length) 0

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
        match resolveVar ctx name true with
        | FieldLoc ->
            emit ctx (LoadLocal 0) 0
            compileExpression ctx expr
            emit ctx (SetMember (name, true)) 0
        | _ ->
            compileExpression ctx expr
            storeRaw ctx name

    | Declaration (_, Dim declarators) ->
        for d in declarators do
            let typeName = d.Type |> Option.map (fun t -> t.Name)
            match d.ArraySpec with
            | Some spec when not spec.Dimensions.IsEmpty ->
                emitDimensions ctx spec.Dimensions
                emit ctx (ArrayNew spec.Dimensions.Length) 0
                emitArrayFill ctx typeName
                storeRaw ctx d.Name
                addSetName ctx.ArrayVars d.Name |> ignore
            | Some _ -> addSetName ctx.ArrayVars d.Name |> ignore
            | None ->
                // Procedure-level declarations are initialised in the prologue;
                // module-level ones once, here.
                if not ctx.InFunction && ctx.ClassFields.IsNone then
                    match typeName with
                    | Some t -> if emitTypedInitial ctx t then storeRaw ctx d.Name
                    | None -> ()

    | Declaration (_, Const (name, _, expr)) ->
        compileExpression ctx expr
        storeRaw ctx name

    | Declaration (_, ReDim (name, dimensions, preserve)) ->
        if preserve then emitLoadVar ctx name
        emitDimensions ctx dimensions
        emit ctx (if preserve then ReDimPreserve dimensions.Length else ArrayNew dimensions.Length) 0
        emitArrayFill ctx (declaredTypeName ctx name)
        storeRaw ctx name
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
        emit ctx Add 0
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

    | LabelStmt label -> setName ctx.Labels label ctx.Instructions.Count

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

    | EraseStmt name ->
        emit ctx LoadEmpty 0
        storeRaw ctx name
        ctx.ArrayVars.Remove(normalizeName name) |> ignore

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

let rec private collectDeclarations (stmt: Statement) (visit: VarDeclarator -> unit) (visitConst: string -> unit) =
    match stmt with
    | LocatedStmt (_, inner) -> collectDeclarations inner visit visitConst
    | Declaration (_, Dim ds) -> for d in ds do visit d
    | Declaration (_, Const (n, _, _)) -> visitConst n
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

let compileFunctionBody (ctx: CompileContext) (name: string) (returnName: string) (paramList: Parameter list) (returnType: TypeRef option) (body: Statement list) =
    let outerLocals = copyNameDictionary ctx.Locals
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
    for stmt in body do
        collectDeclarations stmt (fun d ->
            ignore (getLocalSlot ctx d.Name)
            declared.Add d
            match d.Type, d.ArraySpec with
            | Some t, _ -> setName ctx.LocalTypes d.Name t.Name
            | _ -> ()) (fun n -> ignore (getLocalSlot ctx n))

    let funcIndex = getName ctx.FunctionIndex name
    let startPc = ctx.Instructions.Count

    // Prologue: typed locals and return value start at their type's default;
    // ByVal typed parameters convert the passed value.
    let line = ctx.CurrentLine
    for param in paramList do
        match param.Type with
        | Some t when not param.ByRef && not param.IsArray && not param.IsParamArray && (ValueSemantics.declaredTypeOf t.Name).IsSome ->
            let slot = getName ctx.Locals param.Name
            emit ctx (LoadLocal slot) line
            emitStoreConversion ctx (Some t.Name)
            emit ctx (StoreLocal slot) line
        | _ -> ()
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
        | Some t, None -> initialise (getName ctx.Locals d.Name) t.Name
        | _ -> ()

    for stmt in body do
        compileStatement ctx stmt

    let retSlot = getName ctx.Locals returnName
    emit ctx (LoadLocal retSlot) 0
    emit ctx Return 0

    patchLabels ctx

    let localCount = ctx.NextLocalSlot

    ctx.Locals <- outerLocals
    ctx.LocalTypes <- outerTypes
    ctx.NextLocalSlot <- outerNextLocal
    ctx.ReturnSlotName <- outerReturnSlot
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
        if prefix = "" && ctx.ModuleName <> "" then
            let key = privateKey ctx.ModuleName suffix
            let index = registerFunction ctx key ps
            if kind = StandardModule && not (isPrivate (functionModifiers func)) && not (containsName ctx.FunctionIndex suffix) then
                setName ctx.FunctionIndex suffix index
                setName ctx.Parameters suffix ps
        else
            ignore (registerFunction ctx (prefix + suffix) ps)
    for top in tops do
        match top with
        | FunctionDef f -> register "" f
        | ClassDecl (_, name, members) ->
            if containsName ctx.ClassIndex name then fail ctx (sprintf "Ambiguous name detected: %s" name)
            setName ctx.ClassIndex name ctx.Classes.Count
            ctx.Classes.Add({ Name = name; Fields = [||]; Methods = Map.empty; PropertyGetters = Map.empty; PropertyLetters = Map.empty
                              PropertySetters = Map.empty; IsValueType = false; FieldTypes = [||] })
            for memberDecl in members do
                match memberDecl with FunctionDef f -> register (name + ".") f | _ -> ()
        | TypeDecl (_, name, _) ->
            if containsName ctx.ClassIndex name then fail ctx (sprintf "Ambiguous name detected: %s" name)
            setName ctx.ClassIndex name ctx.Classes.Count
            ctx.Classes.Add({ Name = name; Fields = [||]; Methods = Map.empty; PropertyGetters = Map.empty; PropertyLetters = Map.empty
                              PropertySetters = Map.empty; IsValueType = true; FieldTypes = [||] })
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
        | FunctionDecl (_, name, paramList, returnType, body) -> compileFunctionBody ctx (key name) name paramList returnType body
        | SubDecl (_, name, paramList, body) -> compileFunctionBody ctx (key name) name paramList None body
        | PropertyGet (_, name, paramList, returnType, body) -> compileFunctionBody ctx (key (name + "_get")) name paramList returnType body
        | PropertyLet (_, name, paramList, body) -> compileFunctionBody ctx (key (name + "_let")) name paramList None body
        | PropertySet (_, name, paramList, body) -> compileFunctionBody ctx (key (name + "_set")) name paramList None body
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
                    fields <- (d.Name, (match d.ArraySpec with Some _ -> None | None -> d.Type |> Option.map (fun t -> t.Name))) :: fields
            | _ -> ()

        let fieldSet = newNameSet ()
        for (field, _) in List.rev fields do
            addSetName fieldSet field |> ignore

        for m in members do
            match m with
            | FunctionDef func ->
                ctx.ClassFields <- Some fieldSet
                let compileMember (name: string) suffix paramList returnType body =
                    let fullName = className + "." + name + suffix
                    compileFunctionBody ctx fullName name paramList returnType body
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
        ctx.Classes.[classIdx] <- {
            Name = className
            Fields = orderedFields |> List.map fst |> List.toArray
            Methods = methods
            PropertyGetters = getters
            PropertyLetters = letters
            PropertySetters = setters
            IsValueType = false
            FieldTypes = orderedFields |> List.map (fun (_, t) -> fieldTypeCode ctx t) |> List.toArray
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
                emit ctx Add 0
            storeRaw ctx m.Name
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
    | TopLevelStatement (Declaration (vis, Dim declarators)) ->
        for d in declarators do
            register vis d.Name (d.Type |> Option.map (fun t -> t.Name))
            match d.ArraySpec with Some _ -> addSetName ctx.ArrayVars d.Name |> ignore | None -> ()
    | TopLevelStatement (Declaration (vis, Const (name, _, _))) -> register vis name None
    | TopLevelStatement (Declaration (_, ReDim (name, _, _))) ->
        if (tryResolveGlobal ctx name).IsNone then register None name None
    | EnumDecl (vis, enumName, members) ->
        let names = newNameSet ()
        for m in members do
            register (match vis with None -> Some Public | v -> v) m.Name None
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
    }

/// A module of a project: its name, kind and parsed program.
type CompileModule = {
    Name: string
    Kind: ModuleKind
    Program: Program
}

let private withModule (ctx: CompileContext) (m: CompileModule) action =
    ctx.ModuleName <- m.Name
    ctx.CurrentLine <- 0
    ctx.OptionBase <-
        if m.Program.TopLevels |> List.exists (function OptionStmt o -> normalizeName o = "base 1" | _ -> false) then 1 else 0
    action ()
    ctx.ModuleName <- ""
    ctx.OptionBase <- 0

let private moduleTopLevels (m: CompileModule) =
    match m.Kind with
    | ClassModule ->
        // A class module's body is one class named after the module.
        let classMembers, rest =
            m.Program.TopLevels |> List.partition (function
                | FunctionDef _ | TopLevelStatement _ -> true
                | _ -> false)
        rest @ [ ClassDecl (Some Public, m.Name, classMembers) ]
    | _ -> m.Program.TopLevels

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
    let tops = modules |> List.map (fun m -> m, moduleTopLevels m)
    for m, tls in tops do
        withModule ctx m (fun () -> for tl in tls do preRegisterGlobals ctx tl)
    for m, tls in tops do
        withModule ctx m (fun () -> registerDeclarations ctx m.Kind tls)
    for m, tls in tops do
        withModule ctx m (fun () -> defineTypes ctx tls)
    emit ctx (Jump -1) 0
    for m, tls in tops do
        withModule ctx m (fun () -> for tl in tls do compileFunctions ctx tl)
    patchJump ctx 0 (Jump ctx.Instructions.Count)
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
    compileModules names [ { Name = ""; Kind = StandardModule; Program = program } ] |> fst

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
