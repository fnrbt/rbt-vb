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

// Field order matches the VM's operand-pop order on both native and Fable.
exception ScriptError of description: string * number: int

type ErrorMode =
    | NoHandler
    | ResumeNextMode
    | GoToLabelMode of int

type StackFrame = {
    mutable ReturnAddress: int
    Locals: Value array
    mutable ErrorMode: ErrorMode
    mutable HandlerActive: bool
    mutable StatementEnd: int
    mutable StatementSp: int
    mutable DiscardResult: bool
    GoSubBase: int
}

type VmState = {
    mutable Pc: int
    mutable Stack: Value array
    mutable Sp: int
    mutable CallStack: StackFrame array
    mutable Csp: int
    Globals: Value array
    Constants: Value array
    Program: BytecodeProgram
    GoSubStack: int array
    mutable GoSubSp: int
    mutable LastErrorNumber: int
    mutable LastErrorDescription: string
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

let emptyState (constants: Value array) (program: BytecodeProgram) : VmState =
    let mainStart =
        match program.Code.[0].Opcode with
        | Jump target -> target
        | _ -> 0
    let mutable localCount = 0
    for pc in mainStart .. program.Code.Length - 1 do
        match program.Code.[pc].Opcode with
        | LoadLocal i | StoreLocal i | MakeRefLocal i -> localCount <- max localCount (i + 1)
        | _ -> ()
    let callStack = Array.zeroCreate 16
    callStack.[0] <- {
        ReturnAddress = -1
        Locals = Array.create (max 1 localCount) VEmpty
        ErrorMode = NoHandler; HandlerActive = false; StatementEnd = 0; StatementSp = 0; DiscardResult = false; GoSubBase = 0
    }
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
    }

let inline push (state: VmState) (value: Value) =
    if state.Sp >= state.Stack.Length then
        if state.Sp >= 1048576 then failwith "Stack overflow"
        let grown = Array.create (state.Stack.Length * 2) VEmpty
        System.Array.Copy(state.Stack, grown, state.Stack.Length)
        state.Stack <- grown
    state.Stack.[state.Sp] <- value
    state.Sp <- state.Sp + 1

let inline pop (state: VmState) : Value =
    if state.Sp <= 0 then failwith "Stack underflow"
    state.Sp <- state.Sp - 1
    let value = state.Stack.[state.Sp]
    state.Stack.[state.Sp] <- VEmpty
    value

let inline peek (state: VmState) : Value =
    if state.Sp <= 0 then failwith "Stack underflow"
    state.Stack.[state.Sp - 1]

let inline pushFrame (state: VmState) (returnAddress: int) (localsCount: int) =
    if state.Csp >= state.CallStack.Length then
        if state.Csp >= 4096 then failwith "Call stack overflow"
        let grown = Array.zeroCreate (state.CallStack.Length * 2)
        System.Array.Copy(state.CallStack, grown, state.CallStack.Length)
        state.CallStack <- grown
    state.CallStack.[state.Csp] <- {
        ReturnAddress = returnAddress
        Locals = Array.create (max localsCount 1) VEmpty
        ErrorMode = NoHandler; HandlerActive = false; StatementEnd = 0; StatementSp = state.Sp; DiscardResult = false; GoSubBase = state.GoSubSp
    }
    state.Csp <- state.Csp + 1

let inline popFrame (state: VmState) : StackFrame =
    if state.Csp <= 0 then failwith "Call stack underflow"
    state.Csp <- state.Csp - 1
    let frame = state.CallStack.[state.Csp]
    state.CallStack.[state.Csp] <- Unchecked.defaultof<StackFrame>
    state.GoSubSp <- frame.GoSubBase
    frame

let executeOneInstruction (state: VmState) =
    let instr = state.Program.Code.[state.Pc]

    match instr.Opcode with
    | Statement target ->
        let frame = state.CallStack.[state.Csp - 1]
        frame.StatementEnd <- target
        frame.StatementSp <- state.Sp
        state.Pc <- state.Pc + 1

    | Nop ->
        state.Pc <- state.Pc + 1

    | LoadConst idx ->
        push state state.Constants.[idx]
        state.Pc <- state.Pc + 1

    | LoadNull ->
        push state VNull
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
        let result =
            match left, right with
            | VString l, VString r -> VString (l + r)
            | _ -> numericBinOp (+) (+) left right
        push state result
        state.Pc <- state.Pc + 1

    | Subtract ->
        let right = pop state
        let left = pop state
        push state (numericBinOp (-) (-) left right)
        state.Pc <- state.Pc + 1

    | Multiply ->
        let right = pop state
        let left = pop state
        push state (numericBinOp (*) (*) left right)
        state.Pc <- state.Pc + 1

    | Divide ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else
            let r = valueToDouble right
            if r = 0.0 then failwith "Division by zero"
            push state (VDouble (valueToDouble left / r))
        state.Pc <- state.Pc + 1

    | IntDivide ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else
            let r = valueToInt right
            if r = 0 then failwith "Division by zero"
            push state (VInteger (valueToInt left / r))
        state.Pc <- state.Pc + 1

    | Modulus ->
        let right = pop state
        let left = pop state
        let result =
            if left = VNull || right = VNull then VNull
            else
                let divisor = valueToInt right
                if divisor = 0 then failwith "Division by zero"
                VInteger (valueToInt left % divisor)
        push state result
        state.Pc <- state.Pc + 1

    | Power ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VNull, _ | _, VNull -> VNull
            | VInteger l, VInteger r ->
                let r = float l ** float r
                let ri = int r
                if float ri = r then VInteger ri else VDouble r
            | _ -> VDouble (valueToDouble left ** valueToDouble right)
        push state result
        state.Pc <- state.Pc + 1

    | Concat ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else push state (VString (valueToString left + valueToString right))
        state.Pc <- state.Pc + 1

    | (Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual) as opcode ->
        let right = pop state
        let left = pop state
        let result =
            match ValueSemantics.compareValues state.Program.TextCompare left right with
            | VNull -> VNull
            | VInteger cmp ->
                VBoolean (match opcode with
                          | Equal -> cmp = 0 | NotEqual -> cmp <> 0
                          | LessThan -> cmp < 0 | LessEqual -> cmp <= 0
                          | GreaterThan -> cmp > 0 | _ -> cmp >= 0)
            | _ -> failwith "Invalid comparison"
        push state result
        state.Pc <- state.Pc + 1

    | And ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VNull, VNull -> VNull
            | VNull, value | value, VNull -> if valueToBool value then VNull else VBoolean false
            | _ -> VBoolean (valueToBool left && valueToBool right)
        push state result
        state.Pc <- state.Pc + 1

    | Or ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VNull, VNull -> VNull
            | VNull, value | value, VNull -> if valueToBool value then VBoolean true else VNull
            | _ -> VBoolean (valueToBool left || valueToBool right)
        push state result
        state.Pc <- state.Pc + 1

    | Xor ->
        let right = pop state
        let left = pop state
        if left = VNull || right = VNull then push state VNull
        else push state (VBoolean (valueToBool left <> valueToBool right))
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

    | Not ->
        let value = pop state
        if value = VNull then push state VNull
        else push state (VBoolean (not (valueToBool value)))
        state.Pc <- state.Pc + 1

    | Negate ->
        let value = pop state
        let result =
            match value with
            | VInteger i -> VInteger (-i)
            | VDouble d -> VDouble (-d)
            | VNull -> VNull
            | _ -> VDouble (-valueToDouble value)
        push state result
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
            | _ -> false
        push state (VBoolean result)
        state.Pc <- state.Pc + 1

    | LikeOp ->
        let right = pop state
        let left = pop state
        let text = valueToString left
        let pattern = valueToString right
        push state (VBoolean (vbLikeMatch pattern text))
        state.Pc <- state.Pc + 1

    // Arrays
    | (ArrayNew rank | ReDimPreserve rank) as opcode ->
        let bounds = Array.zeroCreate rank
        let lengths = Array.zeroCreate rank
        for i in rank - 1 .. -1 .. 0 do
            let hi = valueToInt (pop state)
            let lo = valueToInt (pop state)
            let length = int64 hi - int64 lo + 1L
            if length < 0L || length > 1048576L then failwith "Invalid array bounds"
            bounds.[i] <- lo
            lengths.[i] <- int length
        let mutable count = 1L
        for length in lengths do
            count <- count * int64 length
            if count > 1048576L then failwith "Array too large"
        let data = Array.create (int count) VEmpty
        match opcode with
        | ReDimPreserve _ ->
            match pop state with
            | VArray old ->
                if old.Lengths.Length <> rank || old.LowerBounds <> bounds then failwith "Invalid ReDim Preserve bounds"
                for i in 0 .. rank - 2 do
                    if old.Lengths.[i] <> lengths.[i] then failwith "ReDim Preserve can only change last dimension"
                let oldLast = old.Lengths.[rank - 1]
                let newLast = lengths.[rank - 1]
                let rows = if oldLast = 0 then 0 else old.Data.Length / oldLast
                for row in 0 .. rows - 1 do
                    System.Array.Copy(old.Data, row * oldLast, data, row * newLast, min oldLast newLast)
            | VEmpty -> ()
            | _ -> failwith "Expected array"
        | _ -> ()
        push state (VArray { Data = data; LowerBounds = bounds; Lengths = lengths })
        state.Pc <- state.Pc + 1

    | (ArrayGet rank | ArraySet rank | MakeRefArray rank) as opcode ->
        let value = match opcode with ArraySet _ -> pop state | _ -> VEmpty
        let indices = Array.zeroCreate rank
        for i in rank - 1 .. -1 .. 0 do indices.[i] <- valueToInt (pop state)
        match pop state with
        | VArray a ->
            if rank <> a.Lengths.Length then failwith "Array rank mismatch"
            let mutable offset = 0
            for i in 0 .. rank - 1 do
                let index = int64 indices.[i] - int64 a.LowerBounds.[i]
                if index < 0L || index >= int64 a.Lengths.[i] then failwith "Array index out of bounds"
                offset <- offset * a.Lengths.[i] + int index
            match opcode with
            | ArraySet _ -> a.Data.[offset] <- value
            | MakeRefArray _ -> push state (VRef (a.Data, offset))
            | _ -> push state a.Data.[offset]
        | _ -> failwith "Expected array"
        state.Pc <- state.Pc + 1

    | ArrayGetFlat ->
        let index = valueToInt (pop state)
        match pop state with
        | VArray a when index >= 0 && index < a.Data.Length -> push state a.Data.[index]
        | _ -> failwith "Array index out of bounds"
        state.Pc <- state.Pc + 1

    | ArrayLength ->
        match pop state with
        | VArray a -> push state (VInteger a.Data.Length)
        | VString s -> push state (VInteger s.Length)
        | _ -> failwith "Expected array"
        state.Pc <- state.Pc + 1

    // Objects
    | NewObj classIdx ->
        let classDef = state.Program.Classes.[classIdx]
        let fields = Dictionary<string, Value>()
        for fieldName in classDef.Fields do
            fields.[fieldName.ToLowerInvariant()] <- VEmpty
        let obj = VObject { ClassName = classDef.Name; Fields = fields; ClassIndex = classIdx }
        // Auto-call Class_Initialize if it exists
        match Map.tryFind "class_initialize" classDef.Methods with
        | Some funcIdx ->
            let funcDef = state.Program.Functions.[funcIdx]
            pushFrame state (state.Pc + 1) funcDef.LocalsCount
            let frame = state.CallStack.[state.Csp - 1]
            frame.Locals.[0] <- obj  // Me
            // Set return slot to the object so Return pushes it
            let retSlot = 1 + funcDef.ParamsCount
            frame.Locals.[retSlot] <- obj
            state.Pc <- funcDef.StartPC
        | None ->
            push state obj
            state.Pc <- state.Pc + 1

    | (GetMember name | MakeRefMember name) as operation ->
        let obj = pop state
        match obj with
        | VObject vbObj ->
            match vbObj.Fields.TryGetValue(name.ToLowerInvariant()) with
            | true, value ->
                match operation with
                | MakeRefMember _ -> push state (VFieldRef (vbObj, name.ToLowerInvariant()))
                | _ -> push state value
                state.Pc <- state.Pc + 1
            | false, _ ->
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind (name.ToLowerInvariant()) classDef.PropertyGetters with
                | Some funcIdx ->
                    let funcDef = state.Program.Functions.[funcIdx]
                    pushFrame state (state.Pc + 1) funcDef.LocalsCount
                    let frame = state.CallStack.[state.Csp - 1]
                    frame.Locals.[0] <- VObject vbObj
                    state.Pc <- funcDef.StartPC
                | None ->
                    push state VEmpty
                    state.Pc <- state.Pc + 1
        | VHostObject hostObj ->
            hostObj.GetMember name
            |> Option.defaultValue VEmpty
            |> push state
            state.Pc <- state.Pc + 1
        | _ ->
            push state VEmpty
            state.Pc <- state.Pc + 1

    | SetMember (name, isSet) ->
        let value = pop state
        let obj = pop state
        match obj with
        | VObject vbObj ->
            if vbObj.Fields.ContainsKey(name.ToLowerInvariant()) then
                vbObj.Fields.[name.ToLowerInvariant()] <- value
                state.Pc <- state.Pc + 1
            else
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind (name.ToLowerInvariant()) (if isSet then classDef.PropertySetters else classDef.PropertyLetters) with
                | Some funcIdx ->
                    let funcDef = state.Program.Functions.[funcIdx]
                    pushFrame state (state.Pc + 1) funcDef.LocalsCount
                    let frame = state.CallStack.[state.Csp - 1]
                    frame.Locals.[0] <- VObject vbObj
                    frame.Locals.[1] <- value
                    frame.DiscardResult <- true
                    state.Pc <- funcDef.StartPC
                | None ->
                    state.Pc <- state.Pc + 1
        | VHostObject hostObj ->
            ignore (hostObj.SetMember name value)
            state.Pc <- state.Pc + 1
        | _ ->
            state.Pc <- state.Pc + 1

    | CallMethod (name, argCount) ->
        if state.Sp < argCount + 1 then failwith "Stack underflow"
        let receiver = state.Stack.[state.Sp - argCount - 1]
        match receiver with
        | VObject vbObj ->
            let classDef = state.Program.Classes.[vbObj.ClassIndex]
            match Map.tryFind (name.ToLowerInvariant()) classDef.Methods with
            | Some funcIdx ->
                let funcDef = state.Program.Functions.[funcIdx]
                if argCount > funcDef.ParamsCount then failwith "Too many arguments"
                pushFrame state (state.Pc + 1) funcDef.LocalsCount
                let frame = state.CallStack.[state.Csp - 1]
                frame.Locals.[0] <- receiver
                for i in 0 .. argCount - 1 do
                    let value = pop state
                    frame.Locals.[i + 1] <-
                        match value with
                        | VRef (cells, index) when not funcDef.ByRefParams.[i] -> cells.[index]
                        | VFieldRef (obj, field) when not funcDef.ByRefParams.[i] -> obj.Fields.[field]
                        | _ -> value
                ignore (pop state)
                state.Pc <- funcDef.StartPC
            | None -> failwithf "Unknown method %s" name
        | VHostObject hostObj ->
            let args = Array.zeroCreate argCount
            for i in 0 .. argCount - 1 do
                args.[i] <-
                    match pop state with
                    | VRef (cells, index) -> cells.[index]
                    | VFieldRef (obj, field) -> obj.Fields.[field]
                    | value -> value
            ignore (pop state)
            hostObj.CallMethod name args |> Option.defaultValue VEmpty |> push state
            state.Pc <- state.Pc + 1
        | _ -> failwith "Object required"

    | CallDefault argCount ->
        let args = Array.zeroCreate argCount
        for i in 0 .. argCount - 1 do
            args.[i] <- pop state
        let obj = pop state
        let arrayFallback () =
            match obj with
            | VArray a ->
                if argCount <> a.Lengths.Length then failwith "Array rank mismatch"
                let mutable offset = 0
                for i in 0 .. argCount - 1 do
                    let index = int64 (valueToInt args.[i]) - int64 a.LowerBounds.[i]
                    if index < 0L || index >= int64 a.Lengths.[i] then failwith "Array index out of bounds"
                    offset <- offset * a.Lengths.[i] + int index
                a.Data.[offset]
            | _ -> failwith "Object is not callable"
        match obj with
        | VHostObject hostObj ->
            match hostObj.CallMethod "" args with
            | Some value -> push state value
            | None ->
                match hostObj.CallMethod "_Default" args with
                | Some value -> push state value
                | None ->
                    match hostObj.CallMethod "Item" args with
                    | Some value -> push state value
                    | None -> push state (arrayFallback ())
        | _ ->
            push state (arrayFallback ())
        state.Pc <- state.Pc + 1

    | TypeCheck className ->
        let obj = pop state
        match obj with
        | VObject vbObj -> push state (VBoolean (equalsIgnoreCase vbObj.ClassName className))
        | VHostObject hostObj -> push state (VBoolean (equalsIgnoreCase hostObj.TypeName className))
        | _ -> push state (VBoolean false)
        state.Pc <- state.Pc + 1

    // Builtins
    | CallBuiltin (name, argCount) ->
        let args = Array.zeroCreate argCount
        for i in 0 .. argCount - 1 do
            args.[i] <- pop state

        // `name` is already lowercased by the compiler (CallBuiltin emission),
        // so no per-call ToLowerInvariant allocation here.
        let result = Builtins.callBuiltin state.Program.TextCompare name args
        push state result
        state.Pc <- state.Pc + 1

    // Control flow
    | Jump target ->
        state.Pc <- target

    | JumpIfFalse target ->
        let value = pop state
        if value = VNull || not (valueToBool value) then state.Pc <- target
        else state.Pc <- state.Pc + 1

    | JumpIfTrue target ->
        let value = pop state
        if value <> VNull && valueToBool value then state.Pc <- target
        else state.Pc <- state.Pc + 1

    | Call (funcIndex, argCount) ->
        let funcDef = state.Program.Functions.[funcIndex]
        pushFrame state (state.Pc + 1) funcDef.LocalsCount
        let frame = state.CallStack.[state.Csp - 1]
        for i in 0 .. argCount - 1 do
            frame.Locals.[i] <- pop state

        state.Pc <- funcDef.StartPC

    | Return ->
        let retVal = pop state
        let frame = popFrame state
        if not frame.DiscardResult then push state retVal
        state.Pc <- frame.ReturnAddress

    | Bytecode.GoSub target ->
        if state.GoSubSp >= state.GoSubStack.Length then failwith "GoSub stack overflow"
        state.GoSubStack.[state.GoSubSp] <- state.Pc + 1
        state.GoSubSp <- state.GoSubSp + 1
        state.Pc <- target

    | ReturnSub ->
        if state.GoSubSp <= state.CallStack.[state.Csp - 1].GoSubBase then failwith "Return without GoSub"
        state.GoSubSp <- state.GoSubSp - 1
        state.Pc <- state.GoSubStack.[state.GoSubSp]

    | OnErrorResumeNext ->
        state.CallStack.[state.Csp - 1].ErrorMode <- ResumeNextMode
        state.Pc <- state.Pc + 1

    | OnErrorGoToZero ->
        state.CallStack.[state.Csp - 1].ErrorMode <- NoHandler
        state.CallStack.[state.Csp - 1].HandlerActive <- false
        state.Pc <- state.Pc + 1

    | OnErrorGoToLabel target ->
        state.CallStack.[state.Csp - 1].ErrorMode <- GoToLabelMode target
        state.Pc <- state.Pc + 1

    // Err object
    | LoadErrNumber ->
        push state (VInteger state.LastErrorNumber)
        state.Pc <- state.Pc + 1

    | LoadErrDescription ->
        push state (VString state.LastErrorDescription)
        state.Pc <- state.Pc + 1

    | ClearErr ->
        state.LastErrorNumber <- 0
        state.LastErrorDescription <- ""
        push state VEmpty
        state.Pc <- state.Pc + 1

    | RaiseErr ->
        let desc = valueToString (pop state)
        let num = valueToInt (pop state)
        raise (ScriptError (desc, num))

    // ByRef
    | MakeRefLocal slot ->
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | (VRef _ | VFieldRef _) as r -> push state r  // forward existing ref
        | _ -> push state (VRef (frame.Locals, slot))
        state.Pc <- state.Pc + 1

    | MakeRefGlobal slot ->
        push state (VRef (state.Globals, slot))
        state.Pc <- state.Pc + 1

    | Halt ->
        state.Pc <- state.Program.Code.Length

let run (state: VmState) =
    while state.Pc < state.Program.Code.Length do
        try executeOneInstruction state
        with ex ->
            let number, description =
                match ex with ScriptError (d, n) -> n, d | _ -> 11, ex.Message
            state.LastErrorNumber <- number
            state.LastErrorDescription <- description
            let mutable handled = false
            while not handled && state.Csp > 0 do
                let frame = state.CallStack.[state.Csp - 1]
                match frame.ErrorMode with
                | ResumeNextMode when not frame.HandlerActive ->
                    while state.Sp > frame.StatementSp do ignore (pop state)
                    state.Pc <- frame.StatementEnd
                    handled <- true
                | GoToLabelMode target when not frame.HandlerActive ->
                    while state.Sp > frame.StatementSp do ignore (pop state)
                    frame.HandlerActive <- true
                    state.Pc <- target
                    handled <- true
                | _ when state.Csp > 1 ->
                    ignore (popFrame state)
                | _ -> raise ex

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

let executeWithGlobals (globals: seq<string * Value>) (program: BytecodeProgram) : VmState =
    Bytecode.validate program
    let state = emptyState program.Constants program
    setInitialGlobals state globals
    run state
    state

let execute (program: BytecodeProgram) : VmState =
    executeWithGlobals Seq.empty program
