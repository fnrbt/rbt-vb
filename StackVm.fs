module Rbt.Vb.StackVm

open Bytecode
open Values
open System.Collections.Generic

let private equalsIgnoreCase (left: string) (right: string) =
#if FABLE_COMPILER
    let l = if isNull (box left) then "" else left.ToLowerInvariant()
    let r = if isNull (box right) then "" else right.ToLowerInvariant()
    l = r
#else
    System.String.Equals(left, right, System.StringComparison.OrdinalIgnoreCase)
#endif

/// A run-time error that no `On Error` handler caught, with its location.
type RuntimeFailure = {
    Number: int
    Description: string
    Source: string
    /// 1-based source line of the failing statement (0 when unknown).
    Line: int
    /// Procedure name ("" for module-level code).
    Procedure: string
    /// Module name ("" for a single-source program).
    Module: string
    /// What the error was about (VbErrorInfo.Detail).
    Detail: string
}

let formatFailure (failure: RuntimeFailure) =
    let location =
        match failure.Module, failure.Procedure with
        | "", "" -> ""
        | "", proc -> " in " + proc
        | m, "" -> " in " + m
        | m, proc -> " in " + m + "." + proc
    let line = if failure.Line > 0 then sprintf " (line %d%s)" failure.Line location elif location <> "" then " (" + location.Trim() + ")" else ""
    sprintf "Run-time error '%d': %s%s" failure.Number failure.Description line

/// Raised by `run` / `execute` for an unhandled run-time error; the message is
/// the formatted failure.
type VbRuntimeException(failure: RuntimeFailure) =
    inherit exn(formatFailure failure)
    member _.Failure = failure

/// The address a frame entered from the host returns to; it ends `run`.
let hostReturnAddress = System.Int32.MaxValue

type ErrorMode =
    | NoHandler
    | ResumeNextMode
    | GoToLabelMode of int

type StackFrame = {
    mutable ReturnAddress: int
    mutable Locals: Value array
    mutable ErrorMode: ErrorMode
    mutable HandlerActive: bool
    mutable StatementStart: int
    mutable StatementEnd: int
    mutable StatementSp: int
    mutable ResumeStart: int
    mutable ResumeNext: int
    mutable DiscardResult: bool
    mutable GoSubBase: int
    /// Index into Program.Functions, or -1 for module-level / host snippet code.
    mutable FunctionIndex: int
    /// The last numeric line label reached (Erl).
    mutable LineLabel: int
}

type VmState = {
    mutable Pc: int
    mutable Stack: Value array
    mutable Sp: int
    mutable CallStack: StackFrame array
    mutable Csp: int
    mutable Globals: Value array
    mutable Constants: Value array
    mutable Program: BytecodeProgram
    GoSubStack: int array
    mutable GoSubSp: int
    mutable LastErrorNumber: int
    mutable LastErrorDescription: string
    mutable LastErrorSource: string
    mutable Hooks: RuntimeHooks
    /// Err.Source for run-time errors raised by the engine.
    mutable ProjectName: string
    /// Unwinding never pops frames below this depth; an error that reaches it
    /// without a handler is unhandled.
    mutable BaseCsp: int
    mutable CancelRequested: bool
    mutable ErrObject: IHostObject option
    /// Erl of the last error.
    mutable LastErl: int
    /// A `Stop` paused the execution (runSlice returned early).
    mutable Paused: bool
    /// An `End` statement stopped the program.
    mutable EndRequested: bool
    /// Live instances of classes with a Class_Terminate.
    Terminable: ResizeArray<VBObject>
    /// Mark epoch for finding unreachable Terminable objects.
    mutable MarkEpoch: int
    Marks: Dictionary<obj, int>
    /// Lowercased member names, so class-member lookups do not allocate.
    LowerNames: Dictionary<string, string>
    /// Nested runs (callNested) in progress; a yield waits for them.
    mutable NestedDepth: int
    /// Members of host objects by place in the code (HostDispatch): the program they are for, and at each
    /// instruction the type of object, the name and the member's number found there last.
    mutable MemberCacheProgram: obj
    mutable MemberCacheTypes: obj array
    mutable MemberCacheNames: string array
    mutable MemberCacheIds: int array
}

/// Raised by `Stop` to end the current slice.
exception StopSignal

/// Raised at a statement boundary after a host asked to end the slice.
exception YieldSignal

let valueToInt = ValueSemantics.toInteger
let valueToDouble = ValueSemantics.toNumber
let valueToString = ValueSemantics.toString
let valueToBool = ValueSemantics.toBool

type private LikeToken =
    | LikeStar
    | LikeAny
    | LikeDigit
    /// A bracket class: ranges (lo, hi), negated with `!`; `[]` matches nothing.
    | LikeClass of (char * char) list * bool
    | LikeEmpty
    | LikeChar of char

/// `text Like pattern`: `*`, `?`, `#`, `[a-c]`, `[!a-c]`, `[]`; case-sensitive
/// unless Option Compare Text.
let vbLikeMatchWith (textMode: bool) (pattern: string) (text: string) : bool =
    let fold (c: char) = if textMode then System.Char.ToLowerInvariant c else c
    let tokens = ResizeArray<LikeToken>()
    let mutable i = 0
    while i < pattern.Length do
        match pattern.[i] with
        | '*' -> tokens.Add LikeStar; i <- i + 1
        | '?' -> tokens.Add LikeAny; i <- i + 1
        | '#' -> tokens.Add LikeDigit; i <- i + 1
        | '[' ->
            let close = pattern.IndexOf(']', i + 1)
            if close < 0 then raiseVbError 93
            let inner = pattern.Substring(i + 1, close - i - 1)
            if inner = "" then tokens.Add LikeEmpty
            else
                let negate = inner.[0] = '!' && inner.Length > 1
                let body = if negate then inner.Substring 1 else inner
                let ranges = ResizeArray<char * char>()
                let mutable j = 0
                while j < body.Length do
                    if j + 2 < body.Length && body.[j + 1] = '-' then
                        let lo = body.[j]
                        let hi = body.[j + 2]
                        if hi < lo then raiseVbError 93
                        ranges.Add((lo, hi))
                        j <- j + 3
                    else
                        ranges.Add((body.[j], body.[j]))
                        j <- j + 1
                tokens.Add (LikeClass (List.ofSeq ranges, negate))
            i <- close + 1
        | c -> tokens.Add (LikeChar c); i <- i + 1
    let inClass (ranges: (char * char) list) (c: char) =
        ranges |> List.exists (fun (lo, hi) ->
            (c >= lo && c <= hi) || (textMode && (let f = fold c in f >= fold lo && f <= fold hi)))
    let tokenCount = tokens.Count
    // Backtracking match from token t and text position p.
    let rec matchAt t p =
        if t = tokenCount then p = text.Length
        else
            match tokens.[t] with
            | LikeStar ->
                let mutable q = p
                let mutable found = false
                while not found && q <= text.Length do
                    if matchAt (t + 1) q then found <- true
                    q <- q + 1
                found
            | LikeEmpty -> matchAt (t + 1) p
            | _ when p >= text.Length -> false
            | LikeAny -> matchAt (t + 1) (p + 1)
            | LikeDigit -> System.Char.IsDigit text.[p] && matchAt (t + 1) (p + 1)
            | LikeClass (ranges, negate) -> (inClass ranges text.[p] <> negate) && matchAt (t + 1) (p + 1)
            | LikeChar c -> fold c = fold text.[p] && matchAt (t + 1) (p + 1)
    matchAt 0 0

let vbLikeMatch (pattern: string) (text: string) : bool = vbLikeMatchWith false pattern text

let private clearError (state: VmState) =
    state.LastErrorNumber <- 0
    state.LastErrorDescription <- ""
    state.LastErrorSource <- ""
    state.LastErl <- 0

/// Err.Clear: Erl keeps the line of the last error.
let private clearErrKeepErl (state: VmState) =
    let erl = state.LastErl
    clearError state
    state.LastErl <- erl

/// The `Err` object of a VM state.
type ErrObject(state: VmState) =
    interface IHostObject with
        member _.TypeName = "ErrObject"
        member _.GetMember name =
            match name.ToLowerInvariant() with
            | "" | "number" -> Some (VLong state.LastErrorNumber)
            | "description" -> Some (VString state.LastErrorDescription)
            | "source" -> Some (VString state.LastErrorSource)
            | "helpfile" -> Some (VString "")
            | "helpcontext" | "lastdllerror" -> Some (VInteger 0)
            | "clear" -> clearErrKeepErl state; Some VEmpty
            | _ -> None
        member _.SetMember name value =
            match name.ToLowerInvariant() with
            | "" | "number" -> state.LastErrorNumber <- valueToInt value; true
            | "description" -> state.LastErrorDescription <- valueToString value; true
            | "source" -> state.LastErrorSource <- valueToString value; true
            | "helpfile" | "helpcontext" -> true
            | _ -> false
        member _.CallMethod name args =
            let arg i = if i < args.Length then ValueSemantics.dereference args.[i] else VUndefined
            let text i = match arg i with VUndefined | VEmpty -> None | v -> Some (valueToString v)
            match name.ToLowerInvariant() with
            | "clear" -> clearErrKeepErl state; Some VEmpty
            | "raise" ->
                if args.Length < 1 then raiseVbError 449
                let number = valueToInt (arg 0)
                let description =
                    match text 2 with
                    | Some d -> d
                    | None when number = state.LastErrorNumber && state.LastErrorDescription <> "" -> state.LastErrorDescription
                    | None -> errorDescription number
                raise (VbError { Number = number; Description = description; Source = defaultArg (text 1) ""; Detail = "" })
            | "" | "number" when args.Length = 0 -> Some (VLong state.LastErrorNumber)
            | _ -> None
        member this.CallNamed name args _ = (this :> IHostObject).CallMethod name args
        member _.Enumerate () = None

let errObject (state: VmState) =
    match state.ErrObject with
    | Some o -> o
    | None ->
        let o = ErrObject(state) :> IHostObject
        state.ErrObject <- Some o
        o

let newFrameWith returnAddress (locals: Value array) sp goSubBase functionIndex = {
    ReturnAddress = returnAddress
    Locals = locals
    ErrorMode = NoHandler; HandlerActive = false
    StatementStart = 0; StatementEnd = 0; StatementSp = sp
    ResumeStart = 0; ResumeNext = 0
    DiscardResult = false; GoSubBase = goSubBase; FunctionIndex = functionIndex
    LineLabel = 0
}

let newFrame returnAddress localsCount sp goSubBase functionIndex =
    newFrameWith returnAddress (Array.create (max localsCount 1) VEmpty) sp goSubBase functionIndex

let mainRegionStart (program: BytecodeProgram) =
    match program.Code.[0].Opcode with
    | Jump target -> target
    | _ -> 0

let emptyStateWithHooks (hooks: RuntimeHooks) (constants: Value array) (program: BytecodeProgram) : VmState =
    let mainStart = mainRegionStart program
    let mutable localCount = 0
    for pc in mainStart .. program.Code.Length - 1 do
        match program.Code.[pc].Opcode with
        | LoadLocal i | StoreLocal i | MakeRefLocal i -> localCount <- max localCount (i + 1)
        | _ -> ()
    let callStack = Array.zeroCreate 16
    callStack.[0] <- newFrame -1 localCount 0 0 -1
    {
        Pc = 0
        Stack = Array.create 64 VEmpty
        Sp = 0
        CallStack = callStack
        Csp = 1
        Globals = Array.create (max 1 program.Globals) VEmpty
        Constants = constants
        Program = program
        GoSubStack = Array.zeroCreate 64
        GoSubSp = 0
        LastErrorNumber = 0
        LastErrorDescription = ""
        LastErrorSource = ""
        Hooks = hooks
        ProjectName = "VBAProject"
        BaseCsp = 1
        CancelRequested = false
        ErrObject = None
        LastErl = 0
        Paused = false
        EndRequested = false
        Terminable = ResizeArray()
        MarkEpoch = 0
        Marks = Dictionary<obj, int>(HashIdentity.Reference)
        LowerNames = Dictionary<string, string>()
        NestedDepth = 0
        MemberCacheProgram = null
        MemberCacheTypes = [||]
        MemberCacheNames = [||]
        MemberCacheIds = [||]
    }

let emptyState (constants: Value array) (program: BytecodeProgram) : VmState =
    emptyStateWithHooks Builtins.consoleHooks constants program

let inline push (state: VmState) (value: Value) =
    if state.Sp >= state.Stack.Length then
        if state.Sp >= 1048576 then raiseVbError 28
        let grown = Array.create (state.Stack.Length * 2) VEmpty
        System.Array.Copy(state.Stack, grown, state.Stack.Length)
        state.Stack <- grown
    state.Stack.[state.Sp] <- value
    state.Sp <- state.Sp + 1

let inline pop (state: VmState) : Value =
    if state.Sp <= 0 then raiseVbErrorWith 51 "Stack underflow"
    state.Sp <- state.Sp - 1
    let value = state.Stack.[state.Sp]
    state.Stack.[state.Sp] <- VEmpty
    value

let inline peek (state: VmState) : Value =
    if state.Sp <= 0 then raiseVbErrorWith 51 "Stack underflow"
    state.Stack.[state.Sp - 1]

let inline pushFrameFor (state: VmState) (returnAddress: int) (localsCount: int) (functionIndex: int) =
    if state.Csp >= state.CallStack.Length then
        if state.Csp >= 4096 then raiseVbError 28
        let grown = Array.zeroCreate (state.CallStack.Length * 2)
        System.Array.Copy(state.CallStack, grown, state.CallStack.Length)
        state.CallStack <- grown
    let locals =
        if functionIndex >= 0 then
            let template = state.Program.Functions.[functionIndex].InitialLocals
            if template.Length > 0 then
                let locals = Array.zeroCreate template.Length
                Array.blit template 0 locals 0 template.Length
                locals
            else Array.create (max localsCount 1) VEmpty
        else Array.create (max localsCount 1) VEmpty
    state.CallStack.[state.Csp] <- newFrameWith returnAddress locals state.Sp state.GoSubSp functionIndex
    state.Csp <- state.Csp + 1

let inline pushFrame (state: VmState) (returnAddress: int) (localsCount: int) =
    pushFrameFor state returnAddress localsCount -1

let inline popFrame (state: VmState) : StackFrame =
    if state.Csp <= 0 then raiseVbErrorWith 51 "Call stack underflow"
    state.Csp <- state.Csp - 1
    let frame = state.CallStack.[state.Csp]
    state.CallStack.[state.Csp] <- Unchecked.defaultof<StackFrame>
    state.GoSubSp <- frame.GoSubBase
    frame

/// Field dictionaries ignore case on .NET, so `obj.Name` finds field "name"
/// without lowercasing; under Fable keys are lowercase.
let newFieldDictionary () =
#if FABLE_COMPILER
    Dictionary<string, Value>()
#else
    Dictionary<string, Value>(System.StringComparer.OrdinalIgnoreCase)
#endif

let inline private fieldKey (name: string) =
#if FABLE_COMPILER
    name.ToLowerInvariant()
#else
    name
#endif

/// The lowercase form of a member name (class maps are keyed lowercase).
let private lowerName (state: VmState) (name: string) =
#if FABLE_COMPILER
    name.ToLowerInvariant()
#else
    match state.LowerNames.TryGetValue name with
    | true, lower -> lower
    | _ ->
        let lower = name.ToLowerInvariant()
        if state.LowerNames.Count < 4096 then state.LowerNames.[name] <- lower
        lower
#endif

/// A new instance of a class or user-defined type with its declared field defaults.
let rec newInstance (state: VmState) (classIdx: int) =
    let classDef = state.Program.Classes.[classIdx]
    let fields = newFieldDictionary ()
    for i in 0 .. classDef.Fields.Length - 1 do
        let code = if i < classDef.FieldTypes.Length then classDef.FieldTypes.[i] else 0
        fields.[classDef.Fields.[i].ToLowerInvariant()] <-
            if code < 0 then newInstance state (-code - 1)
            elif code = 0 then VEmpty
            else ValueSemantics.defaultOf (enum<ValueSemantics.DeclaredType> code)
    VObject { ClassName = classDef.Name; Fields = fields; ClassIndex = classIdx; Sinks = [] }

let rec private copyValueType (state: VmState) (value: Value) =
    match value with
    | VObject o when state.Program.Classes.[o.ClassIndex].IsValueType ->
        let fields = newFieldDictionary ()
        for kv in o.Fields do fields.[kv.Key] <- copyValueType state kv.Value
        VObject { o with Fields = fields }
    | VArray a ->
        VArray { a with Data = Array.map (copyValueType state) a.Data }
    | other -> other

let private objectError value : 'a =
    match value with
    | VEmpty | VNothing -> raiseVbError 91
    | _ -> raiseVbError 424

let private indexArray (a: VBArray) (args: Value array) =
    if args.Length <> a.Lengths.Length then raiseVbError 9
    let mutable offset = 0
    for i in 0 .. args.Length - 1 do
        let index = int64 (valueToInt args.[i]) - int64 a.LowerBounds.[i]
        if index < 0L || index >= int64 a.Lengths.[i] then raiseVbError 9
        offset <- offset * a.Lengths.[i] + int index
    offset

let private hostArgs (args: Value array) =
    for i in 0 .. args.Length - 1 do
        args.[i] <- ValueSemantics.dereference args.[i]
    args

/// `obj(args)` on a host object: the default member, `_Default`, then `Item`.
let private hostDefaultCall (hostObj: IHostObject) (args: Value array) =
    match hostObj.CallMethod "" args with
    | Some value -> value
    | None ->
        match hostObj.CallMethod "_Default" args with
        | Some value -> value
        | None ->
            match hostObj.CallMethod "Item" args with
            | Some value -> value
            | None ->
                if args.Length = 0 then ValueSemantics.hostDefault hostObj
                else
                    match ValueSemantics.hostDefault hostObj with
                    | VArray a -> a.Data.[indexArray a args]
                    | _ -> raiseVbError 438

/// `obj.Name(args)` on a host object: CallMethod, then GetMember indexed.
let private hostCall (hostObj: IHostObject) (name: string) (args: Value array) =
    match hostObj.CallMethod name args with
    | Some value -> value
    | None ->
        match hostObj.GetMember name with
        | Some value when args.Length = 0 -> value
        | Some (VArray a) -> a.Data.[indexArray a args]
        | Some (VHostObject inner) -> hostDefaultCall inner args
        // (A property that is not a collection, given arguments: VB's "wrong number of arguments".)
        | Some _ -> raiseVbErrorAbout 450 name
        | None -> raiseVbErrorAbout 438 name

let private defaultCall (value: Value) (args: Value array) =
    match value with
    | VArray a when args.Length = 0 -> value
    | VArray a -> a.Data.[indexArray a args]
    | VHostObject hostObj -> hostDefaultCall hostObj (hostArgs args)
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> raiseVbError 13

let private assignDefault (target: Value) (value: Value) =
    match target with
    | VHostObject hostObj ->
        if not (hostObj.SetMember "" value) then raiseVbError 438
    | VObject _ -> raiseVbError 438
    | other -> objectError other

/// `obj(args) = value` on a host object: SetIndexed, else the item's default member.
let private hostIndexedAssign (hostObj: IHostObject) (name: string) (args: Value array) (value: Value) =
    let handled =
        match box hostObj with
        | :? HostObject as h -> h.SetIndexed name args value
        | _ -> false
    if not handled then
        let item = if name = "" then hostDefaultCall hostObj args else hostCall hostObj name args
        assignDefault item (ValueSemantics.dereference value)

let private enterFunction (state: VmState) (funcIdx: int) (receiver: Value) (returnAddress: int) =
    let funcDef = state.Program.Functions.[funcIdx]
    pushFrameFor state returnAddress funcDef.LocalsCount funcIdx
    let frame = state.CallStack.[state.Csp - 1]
    frame.Locals.[0] <- receiver
    state.Pc <- funcDef.StartPC
    frame

/// Call a class method with arguments in a new frame; the result is
/// discarded (Property Let / Set, event handlers).
let private enterWithArgs (state: VmState) (funcIdx: int) (receiver: Value) (args: Value array) =
    let funcDef = state.Program.Functions.[funcIdx]
    if args.Length > funcDef.ParamsCount then raiseVbError 450
    let frame = enterFunction state funcIdx receiver (state.Pc + 1)
    for i in 0 .. funcDef.ParamsCount - 1 do
        frame.Locals.[i + 1] <-
            if i < args.Length then
                match args.[i] with
                | VRef (cells, index) when not funcDef.ByRefParams.[i] -> cells.[index]
                | VFieldRef (obj, field) when not funcDef.ByRefParams.[i] -> obj.Fields.[field]
                | value -> value
            else VUndefined
    frame.DiscardResult <- true
    frame.StatementSp <- state.Sp

let private classOf (state: VmState) (o: VBObject) = state.Program.Classes.[o.ClassIndex]

/// A VB object assigned with Let through its default Property Let.
let private tryLetDefault (state: VmState) (o: VBObject) (args: Value array) (value: Value) =
    let classDef = classOf state o
    if classDef.DefaultMember = "" || classDef.IsValueType then false
    else
        match Map.tryFind classDef.DefaultMember classDef.PropertyLetters with
        | Some funcIdx -> enterWithArgs state funcIdx (VObject o) (Array.append args [| value |]); true
        | None ->
            if args.Length = 0 && o.Fields.ContainsKey classDef.DefaultMember then
                o.Fields.[classDef.DefaultMember] <- value
                state.Pc <- state.Pc + 1
                true
            else false

let private isObjectValue (state: VmState) (value: Value) =
    match value with
    | VHostObject _ | VNothing -> true
    | VObject o -> not (classOf state o).IsValueType
    | _ -> false

/// Let-store into a cell that holds an object: assign its default member
/// (`r = 5` with r a Range). Advances the pc unless a Property Let runs.
let private letStore (state: VmState) (current: Value) (value: Value) (store: Value -> unit) =
    match current with
    | VHostObject hostObj when not (isObjectValue state value) ->
        if not (hostObj.SetMember "" (ValueSemantics.dereference value)) then raiseVbError 438
        state.Pc <- state.Pc + 1
    | VObject o when not (isObjectValue state value) && tryLetDefault state o [||] value -> ()
    | _ ->
        store value
        state.Pc <- state.Pc + 1

/// `target(args) = value` for an array element, a host item or a VB default member.
let private indexedAssign (state: VmState) (target: Value) (args: Value array) (value: Value) =
    match ValueSemantics.dereference target with
    | VArray a ->
        a.Data.[indexArray a args] <- value
        state.Pc <- state.Pc + 1
    | VHostObject hostObj ->
        hostIndexedAssign hostObj "" (hostArgs args) value
        state.Pc <- state.Pc + 1
    | VObject o when tryLetDefault state o args value -> ()
    | VObject _ -> raiseVbError 438
    | other -> objectError other

/// A method, Property Get or field of a VB class instance by name.
let private classCallable (classDef: ClassDef) (key: string) =
    match Map.tryFind key classDef.Methods with
    | Some funcIdx -> Some funcIdx
    | None -> Map.tryFind key classDef.PropertyGetters

/// `receiver.name(args)` with the receiver and arguments on the stack.
/// The number of a host object's member at this instruction: found by name the first time, or when the
/// object is of another type than last time, then remembered (a loop looks no name up after its first pass).
let private memberId (state: VmState) (d: HostDispatch) (name: string) =
    if not (obj.ReferenceEquals(state.MemberCacheProgram, state.Program)) then
        let n = state.Program.Code.Length
        state.MemberCacheProgram <- box state.Program
        state.MemberCacheTypes <- Array.create n null
        state.MemberCacheNames <- Array.create n null
        state.MemberCacheIds <- Array.create n -1
    let pc = state.Pc
    let kind = d.DispatchType
    if pc < 0 || pc >= state.MemberCacheIds.Length then d.Resolve name
    elif obj.ReferenceEquals(state.MemberCacheTypes.[pc], kind) && state.MemberCacheNames.[pc] = name then state.MemberCacheIds.[pc]
    else
        let id = d.Resolve name
        state.MemberCacheTypes.[pc] <- kind
        state.MemberCacheNames.[pc] <- name
        state.MemberCacheIds.[pc] <- id
        id

let rec private callMethod (state: VmState) (name: string) (argCount: int) =
    if state.Sp < argCount + 1 then raiseVbErrorWith 51 "Stack underflow"
    let receiver = state.Stack.[state.Sp - argCount - 1]
    match receiver with
    | VObject vbObj ->
        let classDef = classOf state vbObj
        let key = lowerName state name
        match classCallable classDef key with
        | Some funcIdx ->
            let funcDef = state.Program.Functions.[funcIdx]
            if argCount > funcDef.ParamsCount then raiseVbError 450
            pushFrameFor state (state.Pc + 1) funcDef.LocalsCount funcIdx
            let frame = state.CallStack.[state.Csp - 1]
            for i in argCount - 1 .. -1 .. 0 do
                frame.Locals.[i + 1] <-
                    match pop state with
                    | VRef (cells, index) when not funcDef.ByRefParams.[i] -> cells.[index]
                    | VFieldRef (obj, field) when not funcDef.ByRefParams.[i] -> obj.Fields.[field]
                    | value -> value
            ignore (pop state)
            frame.Locals.[0] <- receiver
            frame.StatementSp <- state.Sp
            for i in argCount .. funcDef.ParamsCount - 1 do
                frame.Locals.[i + 1] <- VUndefined
            state.Pc <- funcDef.StartPC
        | None ->
            match vbObj.Fields.TryGetValue key with
            | true, field ->
                let args = Array.zeroCreate argCount
                for i in argCount - 1 .. -1 .. 0 do args.[i] <- ValueSemantics.dereference (pop state)
                ignore (pop state)
                match field with
                | VObject inner when argCount > 0 && (classOf state inner).DefaultMember <> "" ->
                    // A field holding an object with a default member: obj.field(i).
                    push state field
                    for arg in args do push state arg
                    callMethod state (classOf state inner).DefaultMember argCount
                | _ ->
                    push state (if argCount = 0 then field else defaultCall field args)
                    state.Pc <- state.Pc + 1
            | false, _ -> raiseVbErrorAbout 438 name
    | VHostObject hostObj ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do
            args.[i] <- ValueSemantics.dereference (pop state)
        ignore (pop state)
        let byNumber =
            match box hostObj with
            | :? HostDispatch as d ->
                match memberId state d name with
                | -1 -> None
                | id -> d.CallById id args
            | _ -> None
        push state (match byNumber with Some value -> value | None -> hostCall hostObj name args)
        state.Pc <- state.Pc + 1
    | other -> objectError other

/// `obj.name = value` with the object and value popped.
let private setMember (state: VmState) (obj: Value) (name: string) (value: Value) (isSet: bool) =
    match obj with
    | VObject vbObj ->
        let field = fieldKey name
        if vbObj.Fields.ContainsKey field then
            vbObj.Fields.[field] <- value
            state.Pc <- state.Pc + 1
        else
            let key = lowerName state name
            let classDef = classOf state vbObj
            let handlers = if isSet then classDef.PropertySetters else classDef.PropertyLetters
            match Map.tryFind key handlers with
            | Some funcIdx ->
                let frame = enterFunction state funcIdx obj (state.Pc + 1)
                frame.Locals.[1] <- value
                frame.DiscardResult <- true
            | None -> raiseVbErrorAbout 438 name
    | VHostObject hostObj ->
        let value = ValueSemantics.dereference value
        let byNumber =
            match box hostObj with
            | :? HostDispatch as d ->
                match memberId state d name with
                | -1 -> false
                | id -> d.SetById id value
            | _ -> false
        if not byNumber && not (hostObj.SetMember name value) then
            raiseVbErrorAbout 438 name
        state.Pc <- state.Pc + 1
    | other -> objectError other

/// `obj.name(args) = value`.
let private setMemberIndexed (state: VmState) (obj: Value) (name: string) (args: Value array) (value: Value) =
    match obj with
    | VObject vbObj ->
        let key = lowerName state name
        let classDef = classOf state vbObj
        match Map.tryFind key classDef.PropertyLetters |> Option.orElse (Map.tryFind key classDef.PropertySetters) with
        | Some funcIdx -> enterWithArgs state funcIdx obj (Array.append args [| value |])
        | None ->
            match vbObj.Fields.TryGetValue key with
            | true, field -> indexedAssign state field (Array.map ValueSemantics.dereference args) value
            | _ -> raiseVbErrorAbout 438 name
    | VHostObject hostObj ->
        hostIndexedAssign hostObj name (hostArgs args) value
        state.Pc <- state.Pc + 1
    | other -> objectError other

/// Hooks the VM installs after its dispatcher is defined: run unreachable
/// Class_Terminate handlers (after a frame returns / a Set overwrote an object).
let mutable private collectHook : VmState -> unit = fun _ -> ()

let private hasTerminate (state: VmState) (o: VBObject) =
    let classDef = classOf state o
    not classDef.IsValueType && Map.containsKey "class_terminate" classDef.Methods

/// A value that may hold (or be) an object with a Class_Terminate.
let private mayHoldTerminable (_: VmState) (value: Value) =
    match value with
    | VObject _ -> true
    | VArray _ -> true
    | VHostObject h -> (match box h with :? Collections.VbCollection | :? Collections.VbDictionary -> true | _ -> false)
    | _ -> false

let private frameHoldsTerminable (state: VmState) (frame: StackFrame) =
    let mutable found = false
    for value in frame.Locals do
        if not found && mayHoldTerminable state value then found <- true
    found

/// Convert parameter slots to their declared types (CoerceParams), leaving
/// references to the caller's variables alone.
let private coerceParams (locals: Value array) (pairs: int array) =
    let mutable k = 0
    while k < pairs.Length do
        let slot = pairs.[k]
        let code = pairs.[k + 1]
        match locals.[slot] with
        | VRef _ | VFieldRef _ | VUndefined -> ()
        | VLong _ when code = 2 -> ()
        | VDouble _ when code = 4 -> ()
        | VString _ when code = 6 -> ()
        | VBoolean _ when code = 7 -> ()
        | VInteger i when code = 1 && i >= -32768 && i <= 32767 -> ()
        | VInteger i when code = 2 -> locals.[slot] <- VLong i
        | value -> locals.[slot] <- ValueSemantics.coerceTo (enum<ValueSemantics.DeclaredType> code) value
        k <- k + 2

/// `Call`: a new frame with the arguments; typed parameters convert here
/// rather than in a dispatched CoerceParams instruction.
let private callProcedure (state: VmState) (funcIndex: int) (argCount: int) =
    let funcDef = state.Program.Functions.[funcIndex]
    pushFrameFor state (state.Pc + 1) funcDef.LocalsCount funcIndex
    let frame = state.CallStack.[state.Csp - 1]
    for i in argCount - 1 .. -1 .. 0 do
        frame.Locals.[i] <- pop state
    frame.StatementSp <- state.Sp
    match state.Program.Code.[funcDef.StartPC].Opcode with
    | CoerceParams pairs ->
        coerceParams frame.Locals pairs
        state.Pc <- funcDef.StartPC + 1
    | _ -> state.Pc <- funcDef.StartPC

let private executeCold (state: VmState) (instr: Instruction) =
    match instr.Opcode with
    | Nop ->
        state.Pc <- state.Pc + 1

    | LoadNull ->
        push state VNull
        state.Pc <- state.Pc + 1

    | IntDivide | IntDivideChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.integerDivideWith (match instr.Opcode with IntDivideChecked -> true | _ -> false) false left right)
        state.Pc <- state.Pc + 1

    | Power ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        let result =
            match left, right with
            | VNull, _ | _, VNull -> VNull
            | _ -> VDouble (valueToDouble left ** valueToDouble right)
        push state result
        state.Pc <- state.Pc + 1

    | Xor | Eqv | Imp ->
        let right = pop state
        let left = pop state
        let op = match instr.Opcode with Xor -> ValueSemantics.LXor | Eqv -> ValueSemantics.LEqv | _ -> ValueSemantics.LImp
        push state (ValueSemantics.logicalWith op left right)
        state.Pc <- state.Pc + 1

    | IsOp ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VNothing, VNothing -> true
            | VNull, VNull -> true
            | VObject l, VObject r -> obj.ReferenceEquals(l.Fields, r.Fields)
            | VHostObject l, VHostObject r -> obj.ReferenceEquals(l, r)
            | (VObject _ | VHostObject _ | VNothing), (VObject _ | VHostObject _ | VNothing) -> false
            | VEmpty, _ | _, VEmpty -> false
            | _ -> raiseVbError 424
        push state (VBoolean result)
        state.Pc <- state.Pc + 1

    | LikeOp ->
        let right = pop state
        let left = pop state
        let text = valueToString left
        let pattern = valueToString right
        push state (VBoolean (vbLikeMatchWith state.Program.TextCompare pattern text))
        state.Pc <- state.Pc + 1

    | (ArrayNew rank | ReDimPreserve rank) as opcode ->
        let bounds = Array.zeroCreate rank
        let lengths = Array.zeroCreate rank
        for i in rank - 1 .. -1 .. 0 do
            let hi = valueToInt (pop state)
            let lo = valueToInt (pop state)
            let length = int64 hi - int64 lo + 1L
            if length < 0L then raiseVbError 9
            if length > 16777216L then raiseVbError 7
            bounds.[i] <- lo
            lengths.[i] <- int length
        let mutable count = 1L
        for length in lengths do
            count <- count * int64 length
            if count > 16777216L then raiseVbError 7
        let data = Array.create (int count) VEmpty
        match opcode with
        | ReDimPreserve _ ->
            match pop state with
            | VArray old when old.Lengths.Length = 0 -> ()
            | VArray old ->
                if old.Lengths.Length <> rank then raiseVbError 9
                for i in 0 .. rank - 2 do
                    if old.Lengths.[i] <> lengths.[i] || old.LowerBounds.[i] <> bounds.[i] then raiseVbError 9
                if old.LowerBounds.[rank - 1] <> bounds.[rank - 1] then raiseVbError 9
                let oldLast = old.Lengths.[rank - 1]
                let newLast = lengths.[rank - 1]
                if rank = 1 then
                    System.Array.Copy(old.Data, 0, data, 0, min oldLast newLast)
                else
                    // Data is row-major with the last dimension varying fastest.
                    let rows = if oldLast = 0 then 0 else old.Data.Length / oldLast
                    for row in 0 .. rows - 1 do
                        System.Array.Copy(old.Data, row * oldLast, data, row * newLast, min oldLast newLast)
            | VEmpty -> ()
            | _ -> raiseVbError 13
        | _ -> ()
        push state (VArray { Data = data; LowerBounds = bounds; Lengths = lengths })
        state.Pc <- state.Pc + 1

    | ForEachPrepare ->
        match pop state with
        | VArray _ as array -> push state array
        | VHostObject hostObj ->
            match hostObj.Enumerate () with
            | Some items ->
                let data = Seq.toArray items
                push state (VArray { Data = data; LowerBounds = [| 0 |]; Lengths = [| data.Length |] })
            | None -> raiseVbError 451
        | VObject _ -> raiseVbError 451
        | VNothing -> raiseVbError 91
        | _ -> raiseVbError 13
        state.Pc <- state.Pc + 1

    | ArrayFill constIdx ->
        match peek state with
        | VArray a ->
            let fill = state.Constants.[constIdx]
            for i in 0 .. a.Data.Length - 1 do
                if a.Data.[i] = VEmpty then a.Data.[i] <- fill
        | _ -> ()
        state.Pc <- state.Pc + 1

    | ArrayFillNew classIdx ->
        match peek state with
        | VArray a ->
            for i in 0 .. a.Data.Length - 1 do
                if a.Data.[i] = VEmpty then a.Data.[i] <- newInstance state classIdx
        | _ -> ()
        state.Pc <- state.Pc + 1

    | ArrayReset code ->
        match pop state with
        | VArray a ->
            for i in 0 .. a.Data.Length - 1 do
                a.Data.[i] <-
                    if code = System.Int32.MinValue then VEmpty
                    elif code >= 0 then state.Constants.[code]
                    else newInstance state (-code - 1)
        | _ -> raiseVbError 13
        state.Pc <- state.Pc + 1

    | CopyUdt ->
        match peek state with
        | VObject o when state.Program.Classes.[o.ClassIndex].IsValueType ->
            push state (copyValueType state (pop state))
        | _ -> ()
        state.Pc <- state.Pc + 1

    | SetDefault ->
        let value = pop state
        let target = pop state
        match target with
        | VObject o when tryLetDefault state o [||] value -> ()
        | _ ->
            assignDefault target value
            state.Pc <- state.Pc + 1

    | SetMemberIndexed (name, argCount) ->
        let value = pop state
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do args.[i] <- pop state
        let receiver = pop state
        setMemberIndexed state receiver name args value

    | NewObj classIdx ->
        let classDef = state.Program.Classes.[classIdx]
        let obj = newInstance state classIdx
        match obj with
        | VObject o when Map.containsKey "class_terminate" classDef.Methods -> state.Terminable.Add o
        | _ -> ()
        // Auto-call Class_Initialize if it exists
        match Map.tryFind "class_initialize" classDef.Methods with
        | Some funcIdx ->
            let frame = enterFunction state funcIdx obj (state.Pc + 1)
            // Set return slot to the object so Return pushes it
            let retSlot = 1 + state.Program.Functions.[funcIdx].ParamsCount
            frame.Locals.[retSlot] <- obj
        | None ->
            push state obj
            state.Pc <- state.Pc + 1

    | CallMethodNamed (name, names) ->
        let argCount = names.Length
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do args.[i] <- pop state
        let receiver = pop state
        match receiver with
        | VObject vbObj ->
            let classDef = state.Program.Classes.[vbObj.ClassIndex]
            let key = lowerName state name
            match classCallable classDef key with
            | Some funcIdx ->
                let funcDef = state.Program.Functions.[funcIdx]
                let slots = Array.create funcDef.ParamsCount VUndefined
                for i in 0 .. argCount - 1 do
                    let position =
                        if names.[i] = "" then i
                        else
                            match funcDef.ParamNames |> Array.tryFindIndex (fun p -> equalsIgnoreCase p names.[i]) with
                            | Some p -> p
                            | None -> raiseVbErrorAbout 448 names.[i]
                    if position >= slots.Length then raiseVbError 450
                    slots.[position] <-
                        match args.[i] with
                        | VRef (cells, index) when not funcDef.ByRefParams.[position] -> cells.[index]
                        | VFieldRef (obj, field) when not funcDef.ByRefParams.[position] -> obj.Fields.[field]
                        | value -> value
                let frame = enterFunction state funcIdx receiver (state.Pc + 1)
                for i in 0 .. slots.Length - 1 do frame.Locals.[i + 1] <- slots.[i]
            | None -> raiseVbErrorAbout 438 name
        | VHostObject hostObj ->
            match hostObj.CallNamed name (hostArgs args) names with
            | Some value -> push state value
            | None -> raiseVbErrorAbout 438 name
            state.Pc <- state.Pc + 1
        | other -> objectError other

    | TypeCheck className ->
        let obj = pop state
        match obj with
        | VObject vbObj ->
            let classDef = classOf state vbObj
            push state (VBoolean (equalsIgnoreCase vbObj.ClassName className || classDef.Implements |> Array.exists (equalsIgnoreCase className)))
        | VHostObject hostObj ->
            let typeName = hostObj.TypeName
            let shortName = match typeName.LastIndexOf '.' with -1 -> typeName | i -> typeName.Substring(i + 1)
            let classShort = match className.LastIndexOf '.' with -1 -> className | i -> className.Substring(i + 1)
            push state (VBoolean (equalsIgnoreCase typeName className || equalsIgnoreCase shortName className || equalsIgnoreCase shortName classShort))
        | VNothing -> push state (VBoolean false)
        | _ -> raiseVbError 424
        state.Pc <- state.Pc + 1

    | Bytecode.GoSub target ->
        if state.GoSubSp >= state.GoSubStack.Length then raiseVbError 28
        state.GoSubStack.[state.GoSubSp] <- state.Pc + 1
        state.GoSubSp <- state.GoSubSp + 1
        state.Pc <- target

    | ReturnSub ->
        if state.GoSubSp <= state.CallStack.[state.Csp - 1].GoSubBase then raiseVbError 3
        state.GoSubSp <- state.GoSubSp - 1
        state.Pc <- state.GoSubStack.[state.GoSubSp]

    | OnErrorResumeNext ->
        let frame = state.CallStack.[state.Csp - 1]
        frame.ErrorMode <- ResumeNextMode
        clearError state
        state.Pc <- state.Pc + 1

    | OnErrorGoToZero ->
        let frame = state.CallStack.[state.Csp - 1]
        frame.ErrorMode <- NoHandler
        frame.HandlerActive <- false
        clearError state
        state.Pc <- state.Pc + 1

    | OnErrorGoToMinusOne ->
        state.CallStack.[state.Csp - 1].HandlerActive <- false
        clearError state
        state.Pc <- state.Pc + 1

    | OnErrorGoToLabel target ->
        let frame = state.CallStack.[state.Csp - 1]
        frame.ErrorMode <- GoToLabelMode target
        clearError state
        state.Pc <- state.Pc + 1

    | Resume kind ->
        let frame = state.CallStack.[state.Csp - 1]
        if not frame.HandlerActive then raiseVbError 20
        frame.HandlerActive <- false
        clearError state
        state.Pc <- if kind = 0 then frame.ResumeStart else frame.ResumeNext

    | ResumeLabel target ->
        let frame = state.CallStack.[state.Csp - 1]
        if not frame.HandlerActive then raiseVbError 20
        frame.HandlerActive <- false
        clearError state
        state.Pc <- target

    | LoadErrNumber ->
        push state (VLong state.LastErrorNumber)
        state.Pc <- state.Pc + 1

    | LoadErrDescription ->
        push state (VString state.LastErrorDescription)
        state.Pc <- state.Pc + 1

    | LoadErrSource ->
        push state (VString state.LastErrorSource)
        state.Pc <- state.Pc + 1

    | LoadErrObject ->
        push state (VHostObject (errObject state))
        state.Pc <- state.Pc + 1

    | ClearErr ->
        clearErrKeepErl state
        push state VEmpty
        state.Pc <- state.Pc + 1

    | RaiseErr ->
        let desc = ValueSemantics.dereference (pop state)
        let source = ValueSemantics.dereference (pop state)
        let num = valueToInt (pop state)
        let text value = match value with VUndefined | VEmpty -> "" | v -> valueToString v
        let description = match text desc with "" -> errorDescription num | d -> d
        raise (VbError { Number = num; Description = description; Source = text source; Detail = "" })

    | LineLabel number ->
        state.CallStack.[state.Csp - 1].LineLabel <- number
        state.Pc <- state.Pc + 1

    | LoadErl ->
        push state (VLong state.LastErl)
        state.Pc <- state.Pc + 1

    | StopOp ->
        state.Pc <- state.Pc + 1
        raise StopSignal

    | EndOp ->
        state.EndRequested <- true
        // End does not run Class_Terminate.
        state.Terminable.Clear()
        state.Pc <- hostReturnAddress

    | LetGlobal slot ->
        let value = pop state
        match state.Globals.[slot] with
        | (VHostObject _ | VObject _) as current -> letStore state current value (fun v -> state.Globals.[slot] <- v)
        | _ ->
            state.Globals.[slot] <- value
            state.Pc <- state.Pc + 1

    | EventHandlers eventName ->
        match pop state with
        | VObject source ->
            let entries =
                source.Sinks
                |> List.rev
                |> List.choose (fun (binding, sink) ->
                    match Map.tryFind eventName state.Program.EventBindings.[binding].Handlers with
                    | Some funcIdx -> Some (VArray { Data = [| VInteger funcIdx; sink |]; LowerBounds = [| 0 |]; Lengths = [| 2 |] })
                    | None -> None)
                |> List.toArray
            push state (VArray { Data = entries; LowerBounds = [| 0 |]; Lengths = [| entries.Length |] })
        | _ -> push state (VArray { Data = [||]; LowerBounds = [| 0 |]; Lengths = [| 0 |] })
        state.Pc <- state.Pc + 1

    | CallHandler argCount ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do args.[i] <- pop state
        match pop state with
        | VArray { Data = [| VInteger funcIdx; sink |] } ->
            let funcDef = state.Program.Functions.[funcIdx]
            pushFrameFor state (state.Pc + 1) funcDef.LocalsCount funcIdx
            let frame = state.CallStack.[state.Csp - 1]
            let offset =
                match sink with
                | VObject _ -> frame.Locals.[0] <- sink; 1
                | _ -> 0
            for i in 0 .. funcDef.ParamsCount - 1 do
                frame.Locals.[i + offset] <-
                    if i < argCount then
                        match args.[i] with
                        | VRef (cells, index) when not funcDef.ByRefParams.[i] -> cells.[index]
                        | VFieldRef (obj, field) when not funcDef.ByRefParams.[i] -> obj.Fields.[field]
                        | value -> value
                    else VUndefined
            frame.DiscardResult <- true
            frame.StatementSp <- state.Sp
            state.Pc <- funcDef.StartPC
        | _ -> raiseVbErrorWith 51 "Invalid event handler"

    | BindEvents binding ->
        let sink = pop state
        let old = pop state
        let sameSink a b =
            match a, b with
            | VObject x, VObject y -> obj.ReferenceEquals(x.Fields, y.Fields)
            | VObject _, _ | _, VObject _ -> false
            | _ -> true
        match old with
        | VObject o -> o.Sinks <- o.Sinks |> List.filter (fun (b, s) -> not (b = binding && sameSink s sink))
        | _ -> ()
        match peek state with
        | VObject n when not (n.Sinks |> List.exists (fun (b, s) -> b = binding && sameSink s sink)) ->
            n.Sinks <- (binding, sink) :: n.Sinks
        | _ -> ()
        state.Pc <- state.Pc + 1

    | ReleaseCheck ->
        let old = pop state
        state.Pc <- state.Pc + 1
        if state.Terminable.Count > 0 && mayHoldTerminable state old then collectHook state

    | CallByNameOp argCount ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do args.[i] <- ValueSemantics.dereference (pop state)
        let callType = valueToInt (pop state)
        let name = valueToString (pop state)
        let target = ValueSemantics.dereference (pop state)
        match callType with
        | 1 | 2 ->
            // (VbMethod calls a method only: a property of a host object called so is error 438, as VB's.)
            match callType, target with
            | 1, VHostObject host ->
                match box host with
                | :? HostDispatch as d ->
                    match d.Resolve name with
                    | -1 -> ()
                    | id -> if not (d.IsMethod id) then raiseVbErrorAbout 438 name
                | _ -> ()
            | _ -> ()
            push state target
            for arg in args do push state arg
            callMethod state name argCount
        | 4 | 8 ->
            if argCount = 0 then raiseVbError 449
            let value = args.[argCount - 1]
            let indices = args.[.. argCount - 2]
            // CallByName is an expression: its value is Empty.
            push state VEmpty
            if indices.Length = 0 then setMember state target name value (callType = 8)
            else setMemberIndexed state target name indices value
        | _ -> raiseVbError 5

    | CoerceParams pairs ->
        coerceParams state.CallStack.[state.Csp - 1].Locals pairs
        state.Pc <- state.Pc + 1

    | Halt ->
        state.Pc <- hostReturnAddress
    | _ -> raiseVbErrorWith 51 "Unknown opcode"

let executeOneInstruction (state: VmState) =
    let instr = state.Program.Code.[state.Pc]

    match instr.Opcode with
    | Statement target ->
        // A host call asked to end the Step slice (HostSignals.requestYield).
        if HostSignals.yieldRequested && state.NestedDepth = 0 then raise YieldSignal
        let frame = state.CallStack.[state.Csp - 1]
        frame.StatementStart <- state.Pc
        frame.StatementEnd <- target
        frame.StatementSp <- state.Sp
        state.Pc <- state.Pc + 1

    | LoadConst idx ->
        push state state.Constants.[idx]
        state.Pc <- state.Pc + 1

    | LoadEmpty ->
        push state VEmpty
        state.Pc <- state.Pc + 1

    | LoadTrue ->
        push state (VBoolean true)
        state.Pc <- state.Pc + 1

    | LoadFalse ->
        push state (VBoolean false)
        state.Pc <- state.Pc + 1

    | LoadLocal slot ->
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | VRef (arr, idx) -> push state arr.[idx]  // deref ByRef
        | VFieldRef (obj, name) -> push state obj.Fields.[name]
        | v -> push state v
        state.Pc <- state.Pc + 1

    | StoreLocal slot ->
        let value = pop state
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | VRef (arr, idx) -> arr.[idx] <- value  // write-through ByRef
        | VFieldRef (obj, name) -> obj.Fields.[name] <- value
        | _ -> frame.Locals.[slot] <- value
        state.Pc <- state.Pc + 1

    | LetLocal slot ->
        let value = pop state
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | VRef (arr, idx) ->
            match arr.[idx] with
            | (VHostObject _ | VObject _) as current -> letStore state current value (fun v -> arr.[idx] <- v)
            | _ -> arr.[idx] <- value; state.Pc <- state.Pc + 1
        | VFieldRef (obj, name) ->
            match obj.Fields.[name] with
            | (VHostObject _ | VObject _) as current -> letStore state current value (fun v -> obj.Fields.[name] <- v)
            | _ -> obj.Fields.[name] <- value; state.Pc <- state.Pc + 1
        | (VHostObject _ | VObject _) as current -> letStore state current value (fun v -> frame.Locals.[slot] <- v)
        | _ ->
            frame.Locals.[slot] <- value
            state.Pc <- state.Pc + 1

    | LoadGlobal slot ->
        push state state.Globals.[slot]
        state.Pc <- state.Pc + 1

    | StoreGlobal slot ->
        state.Globals.[slot] <- pop state
        state.Pc <- state.Pc + 1

    | Pop ->
        ignore (pop state)
        state.Pc <- state.Pc + 1

    | Dup ->
        push state (peek state)
        state.Pc <- state.Pc + 1

    | Add ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.addWith false left right)
        state.Pc <- state.Pc + 1

    | AddChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.addWith true left right)
        state.Pc <- state.Pc + 1

    | Subtract | SubtractChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.subtractWith (match instr.Opcode with SubtractChecked -> true | _ -> false) left right)
        state.Pc <- state.Pc + 1

    | Multiply | MultiplyChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.multiplyWith (match instr.Opcode with MultiplyChecked -> true | _ -> false) left right)
        state.Pc <- state.Pc + 1

    | Concat ->
        let right = pop state
        let left = pop state
        match left, right with
        | VString l, VString r ->
            push state (VString (l + r))
            state.Pc <- state.Pc + 1
        | _ ->
        let right = ValueSemantics.scalar right
        let left = ValueSemantics.scalar left
        match left, right with
        | VNull, VNull -> push state VNull
        | VNull, r -> push state (VString (valueToString r))
        | l, VNull -> push state (VString (valueToString l))
        | l, r -> push state (VString (valueToString l + valueToString r))
        state.Pc <- state.Pc + 1

    | (Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual) as opcode ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | (VInteger l | VLong l), (VInteger r | VLong r) ->
                VBoolean (match opcode with
                          | Equal -> l = r | NotEqual -> l <> r
                          | LessThan -> l < r | LessEqual -> l <= r
                          | GreaterThan -> l > r | _ -> l >= r)
            | (VDouble _ | VInteger _ | VLong _), (VDouble _ | VInteger _ | VLong _) ->
                let l = match left with VDouble d -> d | VInteger i | VLong i -> float i | _ -> 0.0
                let r = match right with VDouble d -> d | VInteger i | VLong i -> float i | _ -> 0.0
                VBoolean (match opcode with
                          | Equal -> l = r | NotEqual -> l <> r
                          | LessThan -> l < r | LessEqual -> l <= r
                          | GreaterThan -> l > r | _ -> l >= r)
            | _ ->
            match ValueSemantics.compareValues state.Program.TextCompare left right with
            | VNull -> VNull
            | VInteger cmp ->
                VBoolean (match opcode with
                          | Equal -> cmp = 0 | NotEqual -> cmp <> 0
                          | LessThan -> cmp < 0 | LessEqual -> cmp <= 0
                          | GreaterThan -> cmp > 0 | _ -> cmp >= 0)
            | _ -> raiseVbError 13
        push state result
        state.Pc <- state.Pc + 1

    | (ArrayGet rank | ArraySet rank | MakeRefArray rank) as opcode ->
        let value = match opcode with ArraySet _ -> pop state | _ -> VEmpty
        let indices = Array.zeroCreate rank
        for i in rank - 1 .. -1 .. 0 do indices.[i] <- pop state
        match pop state with
        | VArray a ->
            if rank <> a.Lengths.Length then raiseVbError 9
            let mutable offset = 0
            for i in 0 .. rank - 1 do
                let index = int64 (valueToInt indices.[i]) - int64 a.LowerBounds.[i]
                if index < 0L || index >= int64 a.Lengths.[i] then raiseVbError 9
                offset <- offset * a.Lengths.[i] + int index
            match opcode with
            | ArraySet _ -> a.Data.[offset] <- value
            | MakeRefArray _ -> push state (VRef (a.Data, offset))
            | _ -> push state a.Data.[offset]
            state.Pc <- state.Pc + 1
        | VHostObject hostObj ->
            match opcode with
            | ArraySet _ -> hostIndexedAssign hostObj "" (hostArgs indices) value
            | _ -> push state (hostDefaultCall hostObj (hostArgs indices))
            state.Pc <- state.Pc + 1
        | VObject o as target ->
            match opcode with
            | ArraySet _ -> indexedAssign state target (hostArgs indices) value
            | _ ->
                let dm = (classOf state o).DefaultMember
                if dm = "" then raiseVbError 438
                push state target
                for index in indices do push state (ValueSemantics.dereference index)
                callMethod state dm rank
        | other ->
            match opcode with
            | ArraySet _ -> objectError other
            | _ -> raiseVbError 13

    | Coerce code ->
        match state.Stack.[state.Sp - 1] with
        | VLong _ when code = 2 -> ()
        | VDouble _ when code = 4 -> ()
        | VInteger i when code = 1 && i >= -32768 && i <= 32767 -> ()
        | _ ->
            let value = pop state
            push state (ValueSemantics.coerceTo (enum<ValueSemantics.DeclaredType> code) value)
        state.Pc <- state.Pc + 1

    | (GetMember name | MakeRefMember name) as operation ->
        let obj = pop state
        match obj with
        | VObject vbObj ->
            let field = fieldKey name
            match vbObj.Fields.TryGetValue field with
            | true, value ->
                match operation with
                | MakeRefMember _ -> push state (VFieldRef (vbObj, field))
                | _ -> push state value
                state.Pc <- state.Pc + 1
            | false, _ ->
                let key = lowerName state name
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind key classDef.PropertyGetters with
                | Some funcIdx -> ignore (enterFunction state funcIdx obj (state.Pc + 1))
                | None ->
                    match Map.tryFind key classDef.Methods with
                    | Some funcIdx when state.Program.Functions.[funcIdx].ParamsCount = 0 ->
                        ignore (enterFunction state funcIdx obj (state.Pc + 1))
                    | _ -> raiseVbErrorAbout 438 name
        | VHostObject hostObj ->
            let byNumber =
                match box hostObj with
                | :? HostDispatch as d ->
                    match memberId state d name with
                    | -1 -> None
                    | id -> d.GetById id
                | _ -> None
            let value =
                match byNumber with
                | Some value -> value
                | None ->
                    match hostObj.GetMember name with
                    | Some value -> value
                    | None ->
                        match hostObj.CallMethod name [||] with
                        | Some value -> value
                        | None -> raiseVbErrorAbout 438 name
            push state value
            state.Pc <- state.Pc + 1
        | other -> objectError other

    | SetMember (name, isSet) ->
        let value = pop state
        let obj = pop state
        setMember state obj name value isSet

    | CallMethod (name, argCount) -> callMethod state name argCount

    | CallBuiltin (name, argCount) ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do
            args.[i] <- pop state

        // `name` is already lowercased by the compiler (CallBuiltin emission),
        // so no per-call ToLowerInvariant allocation here.
        let result = Builtins.callBuiltin state.Hooks state.Program.TextCompare name args
        push state result
        state.Pc <- state.Pc + 1

    | Jump target ->
        state.Pc <- target

    | JumpIfFalse target ->
        let value = pop state
        match value with
        | VBoolean b -> if b then state.Pc <- state.Pc + 1 else state.Pc <- target
        | _ ->
            let value = ValueSemantics.scalar value
            if value = VNull || not (valueToBool value) then state.Pc <- target
            else state.Pc <- state.Pc + 1

    | JumpIfTrue target ->
        let value = pop state
        match value with
        | VBoolean b -> if b then state.Pc <- target else state.Pc <- state.Pc + 1
        | _ ->
            let value = ValueSemantics.scalar value
            if value <> VNull && valueToBool value then state.Pc <- target
            else state.Pc <- state.Pc + 1

    | Call (funcIndex, argCount) -> callProcedure state funcIndex argCount

    | Return ->
        let retVal = pop state
        let frame = popFrame state
        if frame.HandlerActive then clearError state
        if not frame.DiscardResult then push state retVal
        state.Pc <- frame.ReturnAddress
        if state.Terminable.Count > 0 && frameHoldsTerminable state frame then collectHook state

    | MakeRefLocal slot ->
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | (VRef _ | VFieldRef _) as r -> push state r  // forward existing ref
        | _ -> push state (VRef (frame.Locals, slot))
        state.Pc <- state.Pc + 1

    | MakeRefGlobal slot ->
        push state (VRef (state.Globals, slot))
        state.Pc <- state.Pc + 1
    | Divide | DivideChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.divideWith (match instr.Opcode with DivideChecked -> true | _ -> false) left right)
        state.Pc <- state.Pc + 1

    | Modulus | ModulusChecked ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.integerDivideWith (match instr.Opcode with ModulusChecked -> true | _ -> false) true left right)
        state.Pc <- state.Pc + 1

    | And ->
        let right = pop state
        let left = pop state
        match left, right with
        | VBoolean l, VBoolean r -> push state (VBoolean (l && r))
        | _ -> push state (ValueSemantics.logicalWith ValueSemantics.LAnd left right)
        state.Pc <- state.Pc + 1

    | Or ->
        let right = pop state
        let left = pop state
        match left, right with
        | VBoolean l, VBoolean r -> push state (VBoolean (l || r))
        | _ -> push state (ValueSemantics.logicalWith ValueSemantics.LOr left right)
        state.Pc <- state.Pc + 1

    | Not ->
        match pop state with
        | VBoolean b -> push state (VBoolean (not b))
        | value -> push state (ValueSemantics.notValue value)
        state.Pc <- state.Pc + 1

    | Negate | NegateChecked ->
        match state.Stack.[state.Sp - 1] with
        | VDouble d -> state.Stack.[state.Sp - 1] <- VDouble (-d)
        | VInteger i when i > -32768 && i <= 32767 -> state.Stack.[state.Sp - 1] <- VInteger (-i)
        | VLong i when i <> System.Int32.MinValue -> state.Stack.[state.Sp - 1] <- VLong (-i)
        | _ -> push state (ValueSemantics.negateWith (match instr.Opcode with NegateChecked -> true | _ -> false) (pop state))
        state.Pc <- state.Pc + 1

    | ArrayGetFlat ->
        let index = valueToInt (pop state)
        match pop state with
        | VArray a when index >= 0 && index < a.Data.Length -> push state a.Data.[index]
        | _ -> raiseVbError 9
        state.Pc <- state.Pc + 1

    | ArrayLength ->
        match pop state with
        | VArray a -> push state (VInteger a.Data.Length)
        | VString s -> push state (VInteger s.Length)
        | _ -> raiseVbError 13
        state.Pc <- state.Pc + 1

    | LetValue ->
        match peek state with
        | VHostObject hostObj ->
            // Let-coercion reads the default property; an object without one
            // is kept so host objects assigned without Set still work.
            let value =
                match hostObj.GetMember "" with
                | Some v -> Some v
                | None ->
                    match hostObj.GetMember "_Default" with
                    | Some v -> Some v
                    | None -> hostObj.GetMember "Value"
            match value with
            | Some v ->
                ignore (pop state)
                push state (match v with VHostObject _ -> ValueSemantics.scalar v | _ -> v)
            | None -> ()
            state.Pc <- state.Pc + 1
        | VObject o when (classOf state o).DefaultMember <> "" && not (classOf state o).IsValueType ->
            // A VB object with a default member: read it (Property Get or method).
            let classDef = classOf state o
            match classCallable classDef classDef.DefaultMember with
            | Some funcIdx ->
                let target = pop state
                let frame = enterFunction state funcIdx target (state.Pc + 1)
                frame.StatementSp <- state.Sp
                for i in 0 .. state.Program.Functions.[funcIdx].ParamsCount - 1 do frame.Locals.[i + 1] <- VUndefined
            | _ ->
                match o.Fields.TryGetValue classDef.DefaultMember with
                | true, v -> ignore (pop state); push state v
                | _ -> ()
                state.Pc <- state.Pc + 1
        | _ -> state.Pc <- state.Pc + 1

    | CallDefault 1 when (match state.Stack.[state.Sp - 2], state.Stack.[state.Sp - 1] with
                          | VArray a, VInteger _ -> a.Lengths.Length = 1
                          | _ -> false) ->
        let index = match pop state with VInteger i -> i | _ -> 0
        match pop state with
        | VArray a ->
            let offset = index - a.LowerBounds.[0]
            if offset < 0 || offset >= a.Lengths.[0] then raiseVbError 9
            push state a.Data.[offset]
        | _ -> ()
        state.Pc <- state.Pc + 1

    | CallDefault argCount ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do
            args.[i] <- ValueSemantics.dereference (pop state)
        let target = ValueSemantics.dereference (pop state)
        match target with
        | VObject o when (classOf state o).DefaultMember <> "" ->
            push state target
            for arg in args do push state arg
            callMethod state (classOf state o).DefaultMember argCount
        | VObject _ when argCount = 0 ->
            push state target
            state.Pc <- state.Pc + 1
        | _ ->
            push state (defaultCall target args)
            state.Pc <- state.Pc + 1

    | _ -> executeCold state instr

/// Location of the instruction at `pc` in the current frame.
let currentLocation (state: VmState) =
    let code = state.Program.Code
    let pc = state.Pc
    let line = if pc >= 0 && pc < code.Length then code.[pc].LineNumber else 0
    let frame = if state.Csp > 0 then state.CallStack.[state.Csp - 1] else Unchecked.defaultof<StackFrame>
    let funcIdx = if state.Csp > 0 then frame.FunctionIndex else -1
    if funcIdx >= 0 && funcIdx < state.Program.Functions.Length then
        let f = state.Program.Functions.[funcIdx]
        let shortName = if f.Module <> "" && f.Name.StartsWith(f.Module + ".") then f.Name.Substring(f.Module.Length + 1) else f.Name
        line, shortName, f.Module
    else
        let mutable moduleName = ""
        for (start, name) in state.Program.ModuleStarts do
            if pc >= start then moduleName <- name
        line, "", moduleName

let private errorInfo (state: VmState) (ex: exn) =
    match ex with
    | VbError info -> info
    | :? VbRuntimeException as e -> { Number = e.Failure.Number; Description = e.Failure.Description; Source = e.Failure.Source; Detail = e.Failure.Detail }
    | _ -> { Number = 440; Description = ex.Message; Source = ""; Detail = "" }

/// Record an error and transfer control to the active handler, or raise
/// `VbRuntimeException` when no handler down to `BaseCsp` takes it.
let dispatchError (state: VmState) (ex: exn) =
    let info = errorInfo state ex
    let line, procedure, moduleName = currentLocation state
    let source = if info.Source = "" then state.ProjectName else info.Source
    state.LastErrorNumber <- info.Number
    state.LastErrorDescription <- info.Description
    state.LastErrorSource <- source
    let mutable handled = false
    while not handled do
        let frame = state.CallStack.[state.Csp - 1]
        // Erl is the line reached in the procedure that handles the error
        // (the line of the call when the error came from a callee).
        state.LastErl <- frame.LineLabel
        match frame.ErrorMode with
        | ResumeNextMode when not frame.HandlerActive ->
            while state.Sp > frame.StatementSp do ignore (pop state)
            state.Pc <- frame.StatementEnd
            handled <- true
        | GoToLabelMode target when not frame.HandlerActive ->
            while state.Sp > frame.StatementSp do ignore (pop state)
            frame.ResumeStart <- frame.StatementStart
            frame.ResumeNext <- frame.StatementEnd
            frame.HandlerActive <- true
            state.Pc <- target
            handled <- true
        | _ when state.Csp > state.BaseCsp ->
            ignore (popFrame state)
        | _ ->
            raise (VbRuntimeException {
                Number = info.Number; Description = info.Description; Source = source
                Line = line; Procedure = procedure; Module = moduleName; Detail = info.Detail })

let cancelledFailure (state: VmState) =
    let line, procedure, moduleName = currentLocation state
    { Number = 18; Description = errorDescription 18; Source = state.ProjectName
      Line = line; Procedure = procedure; Module = moduleName; Detail = "" }

/// Run a VB procedure to its end from inside an instruction (a default
/// member read during arithmetic, a Class_Terminate) and return its value.
/// Errors it does not handle propagate as VbError.
let callNested (state: VmState) (funcIdx: int) (receiver: Value option) (args: Value array) =
    let savedPc = state.Pc
    let savedBase = state.BaseCsp
    let savedCsp = state.Csp
    let savedSp = state.Sp
    let funcDef = state.Program.Functions.[funcIdx]
    pushFrameFor state hostReturnAddress funcDef.LocalsCount funcIdx
    state.NestedDepth <- state.NestedDepth + 1
    let frame = state.CallStack.[state.Csp - 1]
    let offset =
        match receiver with
        | Some r -> frame.Locals.[0] <- r; 1
        | None -> 0
    for i in 0 .. funcDef.ParamsCount - 1 do
        frame.Locals.[i + offset] <- if i < args.Length then args.[i] else VUndefined
    frame.StatementSp <- state.Sp
    state.BaseCsp <- state.Csp
    state.Pc <- funcDef.StartPC
    let unwind () =
        while state.Csp > savedCsp do ignore (popFrame state)
        while state.Sp > savedSp do ignore (pop state)
    try
        try
            while state.Pc <> hostReturnAddress && state.Pc < state.Program.Code.Length do
                try executeOneInstruction state
                with
                | StopSignal -> ()
                | ex -> dispatchError state ex
            // An End or Quit inside ends the nested call only.
            let result = if state.Csp = savedCsp && state.Sp > savedSp then pop state else VEmpty
            unwind ()
            result
        with
        | :? VbRuntimeException as e ->
            unwind ()
            raise (VbError { Number = e.Failure.Number; Description = e.Failure.Description; Source = e.Failure.Source; Detail = e.Failure.Detail })
    finally
        state.NestedDepth <- state.NestedDepth - 1
        state.BaseCsp <- savedBase
        state.Pc <- savedPc

/// The default member of a VB object (Property Get, method or field), read
/// for arithmetic, comparison, concatenation and built-in arguments.
let private objectDefaultOf (state: VmState) (o: VBObject) =
    let classDef = state.Program.Classes.[o.ClassIndex]
    if classDef.DefaultMember = "" || classDef.IsValueType then None
    else
        match Map.tryFind classDef.DefaultMember classDef.PropertyGetters
              |> Option.orElse (Map.tryFind classDef.DefaultMember classDef.Methods) with
        | Some funcIdx -> Some (callNested state funcIdx (Some (VObject o)) [||])
        | None ->
            match o.Fields.TryGetValue classDef.DefaultMember with
            | true, v -> Some v
            | _ -> None

// ── Class_Terminate ──
//
// Objects of classes with a Class_Terminate are registered when created.
// After a procedure returns holding one, after `Set` overwrites one, and when
// a run ends, the registered objects that can no longer be reached from the
// module variables, the stack and the active procedures' locals are
// terminated, each once.

let private markReachable (state: VmState) =
    state.MarkEpoch <- state.MarkEpoch + 1
    let epoch = state.MarkEpoch
    state.Marks.Clear()
    let pending = Stack<Value>()
    let visit (value: Value) =
        match value with
        | VObject _ | VArray _ | VRef _ | VFieldRef _ | VHostObject _ -> pending.Push value
        | _ -> ()
    let markKey (key: obj) =
        if state.Marks.ContainsKey key then false
        else
            state.Marks.[key] <- epoch
            true
    for g in state.Globals do visit g
    for i in 0 .. state.Sp - 1 do visit state.Stack.[i]
    for f in 0 .. state.Csp - 1 do
        let frame = state.CallStack.[f]
        if not (obj.ReferenceEquals(frame, null)) then
            for l in frame.Locals do visit l
    while pending.Count > 0 do
        match pending.Pop() with
        | VObject o ->
            if markKey (box o.Fields) then
                for kv in o.Fields do visit kv.Value
                for (_, sink) in o.Sinks do visit sink
        | VArray a -> if markKey (box a.Data) then for item in a.Data do visit item
        | VRef (cells, index) -> visit cells.[index]
        | VFieldRef (o, _) -> visit (VObject o)
        | VHostObject h ->
            if markKey (box h) then
                match box h with
                | :? Collections.VbCollection as c -> for item in c.Items do visit item
                | :? Collections.VbDictionary as d ->
                    for item in d.Keys do visit item
                    for item in d.Items do visit item
                | _ -> ()
        | _ -> ()

let private terminate (state: VmState) (objects: VBObject list) =
    for o in objects do
        match Map.tryFind "class_terminate" state.Program.Classes.[o.ClassIndex].Methods with
        | Some funcIdx -> ignore (callNested state funcIdx (Some (VObject o)) [||])
        | None -> ()

/// Terminate the registered objects that are no longer reachable.
let collectUnreachable (state: VmState) =
    if state.Terminable.Count > 0 then
        markReachable state
        let dead = state.Terminable |> Seq.filter (fun o -> not (state.Marks.ContainsKey (box o.Fields))) |> List.ofSeq
        state.Marks.Clear()
        if not dead.IsEmpty then
            let deadSet = HashSet<obj>(HashIdentity.Reference)
            for o in dead do deadSet.Add(box o.Fields) |> ignore
            state.Terminable.RemoveAll(fun o -> deadSet.Contains(box o.Fields)) |> ignore
            terminate state dead

/// End of a run: terminate every object still registered, oldest first.
/// Errors in Class_Terminate are ignored here.
let finalizeObjects (state: VmState) =
    while state.Terminable.Count > 0 do
        let objects = List.ofSeq state.Terminable
        state.Terminable.Clear()
        for o in objects do
            try terminate state [ o ] with _ -> ()

collectHook <- collectUnreachable

/// Execute at most `budget` instructions. Returns true when the program (or
/// the host-entered frame) has finished. Raises `VbRuntimeException` for an
/// unhandled error, including cancellation (error 18). A `Stop` ends the
/// slice early with `Paused` set.
let runSlice (state: VmState) (budget: int) =
    HostSignals.yieldRequested <- false
    let previousDefault = ValueSemantics.objectDefault
    let hasDefaults = state.Program.Classes |> Array.exists (fun c -> c.DefaultMember <> "")
    if hasDefaults then ValueSemantics.objectDefault <- objectDefaultOf state
    try
        let mutable remaining = budget
        while remaining > 0 && state.Pc < state.Program.Code.Length do
            if state.CancelRequested then
                state.CancelRequested <- false
                raise (VbRuntimeException (cancelledFailure state))
            let chunk = min remaining 8192
            let mutable executed = 0
            try
                let code = state.Program.Code
                while executed < chunk && state.Pc < code.Length do
                    executeOneInstruction state
                    executed <- executed + 1
            with
            | StopSignal ->
                executed <- executed + 1
                state.Paused <- true
                remaining <- 0
            | YieldSignal ->
                HostSignals.yieldRequested <- false
                remaining <- 0
            | ex ->
                executed <- executed + 1
                dispatchError state ex
            remaining <- remaining - executed
        state.Pc >= state.Program.Code.Length
    finally
        if hasDefaults then ValueSemantics.objectDefault <- previousDefault

let run (state: VmState) =
    while not (runSlice state System.Int32.MaxValue) do
        state.Paused <- false
    state.Paused <- false
    if not state.EndRequested then finalizeObjects state
    Builtins.flushPendingPrint state.Hooks

let private setInitialGlobals (state: VmState) (globals: seq<string * Value>) =
    for name, value in globals do
        let slot =
            state.Program.GlobalNames
            |> Array.tryFindIndex (fun globalName ->
                equalsIgnoreCase globalName name)
        match slot with
        | Some idx when idx >= 0 && idx < state.Globals.Length ->
            state.Globals.[idx] <- value
        | _ -> ()

let createState (hooks: RuntimeHooks) (globals: seq<string * Value>) (program: BytecodeProgram) : VmState =
    Bytecode.validate program
    let state = emptyStateWithHooks hooks program.Constants program
    setInitialGlobals state globals
    state

let executeWithGlobals (globals: seq<string * Value>) (program: BytecodeProgram) : VmState =
    let state = createState Builtins.consoleHooks globals program
    run state
    state

let execute (program: BytecodeProgram) : VmState =
    executeWithGlobals Seq.empty program
