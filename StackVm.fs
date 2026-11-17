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
}

let valueToInt = ValueSemantics.toInteger
let valueToDouble = ValueSemantics.toNumber
let valueToString = ValueSemantics.toString
let valueToBool = ValueSemantics.toBool
let numericBinOp = ValueSemantics.numericBinOp
let numericCmp = ValueSemantics.numericCmp

let vbLikeMatch (pattern: string) (text: string) : bool =
    let classContains (chars: string) (ch: char) =
        let target = System.Char.ToLowerInvariant ch
        let mutable found = false
        let mutable i = 0
        while not found && i < chars.Length do
            if System.Char.ToLowerInvariant chars.[i] = target then
                found <- true
            i <- i + 1
        found

    let tokenMatches pi ti =
        if pi >= pattern.Length || ti >= text.Length then
            ValueNone
        else
            match pattern.[pi] with
            | '*' -> ValueSome pi
            | '?' -> ValueSome (pi + 1)
            | '#' ->
                if System.Char.IsDigit(text.[ti]) then ValueSome (pi + 1) else ValueNone
            | '[' ->
                let mutable endBracket = pi + 1
                while endBracket < pattern.Length && pattern.[endBracket] <> ']' do
                    endBracket <- endBracket + 1
                if endBracket >= pattern.Length then
                    ValueNone
                else
                    let innerStart = pi + 1
                    let negate = innerStart < endBracket && pattern.[innerStart] = '!'
                    let charsStart = if negate then innerStart + 1 else innerStart
                    let charsLength = endBracket - charsStart
                    let chars = if charsLength <= 0 then "" else pattern.Substring(charsStart, charsLength)
                    let found = classContains chars text.[ti]
                    let matches = if negate then not found else found
                    if matches then ValueSome (endBracket + 1) else ValueNone
            | c ->
                if System.Char.ToLowerInvariant c = System.Char.ToLowerInvariant text.[ti] then
                    ValueSome (pi + 1)
                else
                    ValueNone

    let mutable pi = 0
    let mutable ti = 0
    let mutable starPi = -1
    let mutable starTextNext = -1
    let mutable matched = true
    while matched && ti < text.Length do
        if pi < pattern.Length && pattern.[pi] = '*' then
            starPi <- pi
            pi <- pi + 1
            starTextNext <- ti
        else
            match tokenMatches pi ti with
            | ValueSome nextPi ->
                pi <- nextPi
                ti <- ti + 1
            | ValueNone ->
                if starPi >= 0 then
                    pi <- starPi + 1
                    starTextNext <- starTextNext + 1
                    ti <- starTextNext
                else
                    matched <- false
    while matched && pi < pattern.Length && pattern.[pi] = '*' do
        pi <- pi + 1
    matched && pi >= pattern.Length

let private clearError (state: VmState) =
    state.LastErrorNumber <- 0
    state.LastErrorDescription <- ""
    state.LastErrorSource <- ""

/// The `Err` object of a VM state.
type ErrObject(state: VmState) =
    interface IHostObject with
        member _.TypeName = "ErrObject"
        member _.GetMember name =
            match name.ToLowerInvariant() with
            | "" | "number" -> Some (VInteger state.LastErrorNumber)
            | "description" -> Some (VString state.LastErrorDescription)
            | "source" -> Some (VString state.LastErrorSource)
            | "helpfile" -> Some (VString "")
            | "helpcontext" | "lastdllerror" -> Some (VInteger 0)
            | "clear" -> clearError state; Some VEmpty
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
            | "clear" -> clearError state; Some VEmpty
            | "raise" ->
                if args.Length < 1 then raiseVbError 449
                let number = valueToInt (arg 0)
                let description =
                    match text 2 with
                    | Some d -> d
                    | None when number = state.LastErrorNumber && state.LastErrorDescription <> "" -> state.LastErrorDescription
                    | None -> errorDescription number
                raise (VbError { Number = number; Description = description; Source = defaultArg (text 1) "" })
            | "" | "number" when args.Length = 0 -> Some (VInteger state.LastErrorNumber)
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

/// A new instance of a class or user-defined type with its declared field defaults.
let rec newInstance (state: VmState) (classIdx: int) =
    let classDef = state.Program.Classes.[classIdx]
    let fields = Dictionary<string, Value>()
    for i in 0 .. classDef.Fields.Length - 1 do
        let code = if i < classDef.FieldTypes.Length then classDef.FieldTypes.[i] else 0
        fields.[classDef.Fields.[i].ToLowerInvariant()] <-
            if code < 0 then newInstance state (-code - 1)
            elif code = 0 then VEmpty
            else ValueSemantics.defaultOf (enum<ValueSemantics.DeclaredType> code)
    VObject { ClassName = classDef.Name; Fields = fields; ClassIndex = classIdx }

let rec private copyValueType (state: VmState) (value: Value) =
    match value with
    | VObject o when state.Program.Classes.[o.ClassIndex].IsValueType ->
        let fields = Dictionary<string, Value>()
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

let private enterFunction (state: VmState) (funcIdx: int) (receiver: Value) (returnAddress: int) =
    let funcDef = state.Program.Functions.[funcIdx]
    pushFrameFor state returnAddress funcDef.LocalsCount funcIdx
    let frame = state.CallStack.[state.Csp - 1]
    frame.Locals.[0] <- receiver
    state.Pc <- funcDef.StartPC
    frame

let private executeCold (state: VmState) (instr: Instruction) =
    match instr.Opcode with
    | Nop ->
        state.Pc <- state.Pc + 1

    | LoadNull ->
        push state VNull
        state.Pc <- state.Pc + 1

    | IntDivide ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        if left = VNull || right = VNull then push state VNull
        else
            let r = valueToInt right
            if r = 0 then raiseVbError 11
            let l = valueToInt left
            if l = System.Int32.MinValue && r = -1 then raiseVbError 6
            push state (VInteger (l / r))
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

    | Xor ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        match left, right with
        | VNull, _ | _, VNull -> push state VNull
        | VInteger l, VInteger r -> push state (VInteger (l ^^^ r))
        | _ -> push state (VBoolean (valueToBool left <> valueToBool right))
        state.Pc <- state.Pc + 1

    | Eqv ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else push state (VBoolean (valueToBool left = valueToBool right))
        state.Pc <- state.Pc + 1

    | Imp ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else push state (VBoolean (not (valueToBool left) || valueToBool right))
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
        push state (VBoolean (vbLikeMatch pattern text))
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

    | CopyUdt ->
        match peek state with
        | VObject o when state.Program.Classes.[o.ClassIndex].IsValueType ->
            push state (copyValueType state (pop state))
        | _ -> ()
        state.Pc <- state.Pc + 1

    | SetDefault ->
        let value = pop state
        let target = pop state
        assignDefault target value
        state.Pc <- state.Pc + 1

    | NewObj classIdx ->
        let classDef = state.Program.Classes.[classIdx]
        let obj = newInstance state classIdx
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
            let key = name.ToLowerInvariant()
            let callable =
                match Map.tryFind key classDef.Methods with
                | Some funcIdx -> Some funcIdx
                | None -> Map.tryFind key classDef.PropertyGetters
            match callable with
            | Some funcIdx ->
                let funcDef = state.Program.Functions.[funcIdx]
                let slots = Array.create funcDef.ParamsCount VUndefined
                for i in 0 .. argCount - 1 do
                    let position =
                        if names.[i] = "" then i
                        else
                            match funcDef.ParamNames |> Array.tryFindIndex (fun p -> equalsIgnoreCase p names.[i]) with
                            | Some p -> p
                            | None -> raiseVbErrorWith 448 ("Named argument not found: " + names.[i])
                    if position >= slots.Length then raiseVbError 450
                    slots.[position] <-
                        match args.[i] with
                        | VRef (cells, index) when not funcDef.ByRefParams.[position] -> cells.[index]
                        | VFieldRef (obj, field) when not funcDef.ByRefParams.[position] -> obj.Fields.[field]
                        | value -> value
                let frame = enterFunction state funcIdx receiver (state.Pc + 1)
                for i in 0 .. slots.Length - 1 do frame.Locals.[i + 1] <- slots.[i]
            | None -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
        | VHostObject hostObj ->
            match hostObj.CallNamed name (hostArgs args) names with
            | Some value -> push state value
            | None -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
            state.Pc <- state.Pc + 1
        | other -> objectError other

    | CallDefault argCount ->
        let args = Array.zeroCreate argCount
        for i in argCount - 1 .. -1 .. 0 do
            args.[i] <- ValueSemantics.dereference (pop state)
        let target = ValueSemantics.dereference (pop state)
        push state (defaultCall target args)
        state.Pc <- state.Pc + 1

    | TypeCheck className ->
        let obj = pop state
        match obj with
        | VObject vbObj -> push state (VBoolean (equalsIgnoreCase vbObj.ClassName className))
        | VHostObject hostObj ->
            let typeName = hostObj.TypeName
            let shortName = match typeName.LastIndexOf '.' with -1 -> typeName | i -> typeName.Substring(i + 1)
            push state (VBoolean (equalsIgnoreCase typeName className || equalsIgnoreCase shortName className))
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
        push state (VInteger state.LastErrorNumber)
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
        clearError state
        push state VEmpty
        state.Pc <- state.Pc + 1

    | RaiseErr ->
        let desc = ValueSemantics.dereference (pop state)
        let source = ValueSemantics.dereference (pop state)
        let num = valueToInt (pop state)
        let text value = match value with VUndefined | VEmpty -> "" | v -> valueToString v
        let description = match text desc with "" -> errorDescription num | d -> d
        raise (VbError { Number = num; Description = description; Source = text source })

    | Halt ->
        state.Pc <- hostReturnAddress
    | _ -> raiseVbErrorWith 51 "Unknown opcode"

let executeOneInstruction (state: VmState) =
    let instr = state.Program.Code.[state.Pc]

    match instr.Opcode with
    | Statement target ->
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
        push state (ValueSemantics.addValues left right)
        state.Pc <- state.Pc + 1

    | Subtract ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.subtractValues left right)
        state.Pc <- state.Pc + 1

    | Multiply ->
        let right = pop state
        let left = pop state
        push state (ValueSemantics.multiplyValues left right)
        state.Pc <- state.Pc + 1

    | Concat ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
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
            | VInteger l, VInteger r ->
                VBoolean (match opcode with
                          | Equal -> l = r | NotEqual -> l <> r
                          | LessThan -> l < r | LessEqual -> l <= r
                          | GreaterThan -> l > r | _ -> l >= r)
            | (VDouble _ | VInteger _), (VDouble _ | VInteger _) ->
                let l = match left with VDouble d -> d | VInteger i -> float i | _ -> 0.0
                let r = match right with VDouble d -> d | VInteger i -> float i | _ -> 0.0
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
        | VHostObject hostObj ->
            let item = hostDefaultCall hostObj (hostArgs indices)
            match opcode with
            | ArraySet _ -> assignDefault item value
            | _ -> push state item
        | other ->
            match opcode with
            | ArraySet _ -> objectError other
            | _ -> raiseVbError 13
        state.Pc <- state.Pc + 1

    | Coerce code ->
        match state.Stack.[state.Sp - 1] with
        | VInteger _ when code = 2 -> ()
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
            let key = name.ToLowerInvariant()
            match vbObj.Fields.TryGetValue key with
            | true, value ->
                match operation with
                | MakeRefMember _ -> push state (VFieldRef (vbObj, key))
                | _ -> push state value
                state.Pc <- state.Pc + 1
            | false, _ ->
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind key classDef.PropertyGetters with
                | Some funcIdx -> ignore (enterFunction state funcIdx obj (state.Pc + 1))
                | None ->
                    match Map.tryFind key classDef.Methods with
                    | Some funcIdx when state.Program.Functions.[funcIdx].ParamsCount = 0 ->
                        ignore (enterFunction state funcIdx obj (state.Pc + 1))
                    | _ -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
        | VHostObject hostObj ->
            match hostObj.GetMember name with
            | Some value -> push state value
            | None ->
                match hostObj.CallMethod name [||] with
                | Some value -> push state value
                | None -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
            state.Pc <- state.Pc + 1
        | other -> objectError other

    | SetMember (name, isSet) ->
        let value = pop state
        let obj = pop state
        match obj with
        | VObject vbObj ->
            let key = name.ToLowerInvariant()
            if vbObj.Fields.ContainsKey key then
                vbObj.Fields.[key] <- value
                state.Pc <- state.Pc + 1
            else
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                let handlers = if isSet then classDef.PropertySetters else classDef.PropertyLetters
                match Map.tryFind key handlers with
                | Some funcIdx ->
                    let frame = enterFunction state funcIdx obj (state.Pc + 1)
                    frame.Locals.[1] <- value
                    frame.DiscardResult <- true
                | None -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
        | VHostObject hostObj ->
            if not (hostObj.SetMember name (ValueSemantics.dereference value)) then
                raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
            state.Pc <- state.Pc + 1
        | other -> objectError other

    | CallMethod (name, argCount) ->
        if state.Sp < argCount + 1 then raiseVbErrorWith 51 "Stack underflow"
        let receiver = state.Stack.[state.Sp - argCount - 1]
        match receiver with
        | VObject vbObj ->
            let classDef = state.Program.Classes.[vbObj.ClassIndex]
            let key = name.ToLowerInvariant()
            let callable =
                match Map.tryFind key classDef.Methods with
                | Some funcIdx -> Some funcIdx
                | None -> Map.tryFind key classDef.PropertyGetters
            match callable with
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
                    push state (if argCount = 0 then field else defaultCall field args)
                    state.Pc <- state.Pc + 1
                | false, _ -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
        | VHostObject hostObj ->
            let args = Array.zeroCreate argCount
            for i in argCount - 1 .. -1 .. 0 do
                args.[i] <- ValueSemantics.dereference (pop state)
            ignore (pop state)
            let result =
                match hostObj.CallMethod name args with
                | Some value -> value
                | None ->
                    match hostObj.GetMember name with
                    | Some value when argCount = 0 -> value
                    | Some value -> defaultCall value args
                    | None -> raiseVbErrorWith 438 (errorDescription 438 + ": " + name)
            push state result
            state.Pc <- state.Pc + 1
        | other -> objectError other

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

    | Call (funcIndex, argCount) ->
        let funcDef = state.Program.Functions.[funcIndex]
        pushFrameFor state (state.Pc + 1) funcDef.LocalsCount funcIndex
        let frame = state.CallStack.[state.Csp - 1]
        for i in argCount - 1 .. -1 .. 0 do
            frame.Locals.[i] <- pop state
        frame.StatementSp <- state.Sp
        state.Pc <- funcDef.StartPC

    | Return ->
        let retVal = pop state
        let frame = popFrame state
        if frame.HandlerActive then clearError state
        if not frame.DiscardResult then push state retVal
        state.Pc <- frame.ReturnAddress

    | MakeRefLocal slot ->
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | (VRef _ | VFieldRef _) as r -> push state r  // forward existing ref
        | _ -> push state (VRef (frame.Locals, slot))
        state.Pc <- state.Pc + 1

    | MakeRefGlobal slot ->
        push state (VRef (state.Globals, slot))
        state.Pc <- state.Pc + 1
    | Divide ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        if left = VNull || right = VNull then push state VNull
        else
            let r = valueToDouble right
            if r = 0.0 then raiseVbError 11
            push state (VDouble (valueToDouble left / r))
        state.Pc <- state.Pc + 1

    | Modulus ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        let result =
            if left = VNull || right = VNull then VNull
            else
                let divisor = valueToInt right
                if divisor = 0 then raiseVbError 11
                if divisor = -1 then VInteger 0
                else VInteger (valueToInt left % divisor)
        push state result
        state.Pc <- state.Pc + 1

    | And ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        let result =
            match left, right with
            | VNull, VNull -> VNull
            | VNull, value | value, VNull -> if valueToBool value then VNull else VBoolean false
            | VInteger l, VInteger r -> VInteger (l &&& r)
            | _ -> VBoolean (valueToBool left && valueToBool right)
        push state result
        state.Pc <- state.Pc + 1

    | Or ->
        let right = ValueSemantics.scalar (pop state)
        let left = ValueSemantics.scalar (pop state)
        let result =
            match left, right with
            | VNull, VNull -> VNull
            | VNull, value | value, VNull -> if valueToBool value then VBoolean true else VNull
            | VInteger l, VInteger r -> VInteger (l ||| r)
            | _ -> VBoolean (valueToBool left || valueToBool right)
        push state result
        state.Pc <- state.Pc + 1

    | Not ->
        let value = ValueSemantics.scalar (pop state)
        match value with
        | VNull -> push state VNull
        | VInteger i -> push state (VInteger (~~~i))
        | _ -> push state (VBoolean (not (valueToBool value)))
        state.Pc <- state.Pc + 1

    | Negate ->
        match state.Stack.[state.Sp - 1] with
        | VDouble d -> state.Stack.[state.Sp - 1] <- VDouble (-d)
        | VInteger i when i <> System.Int32.MinValue -> state.Stack.[state.Sp - 1] <- VInteger (-i)
        | _ -> push state (ValueSemantics.negateValue (pop state))
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
        | _ -> ()
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
    | :? VbRuntimeException as e -> { Number = e.Failure.Number; Description = e.Failure.Description; Source = e.Failure.Source }
    | _ -> { Number = 440; Description = ex.Message; Source = "" }

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
                Line = line; Procedure = procedure; Module = moduleName })

let cancelledFailure (state: VmState) =
    let line, procedure, moduleName = currentLocation state
    { Number = 18; Description = errorDescription 18; Source = state.ProjectName
      Line = line; Procedure = procedure; Module = moduleName }

/// Execute at most `budget` instructions. Returns true when the program (or
/// the host-entered frame) has finished. Raises `VbRuntimeException` for an
/// unhandled error, including cancellation (error 18).
let runSlice (state: VmState) (budget: int) =
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
        with ex ->
            executed <- executed + 1
            dispatchError state ex
        remaining <- remaining - executed
    state.Pc >= state.Program.Code.Length

let run (state: VmState) =
    while not (runSlice state System.Int32.MaxValue) do ()

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
