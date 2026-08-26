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

type ErrorMode =
    | NoHandler
    | ResumeNextMode
    | GoToLabelMode of int

type StackFrame = {
    mutable ReturnAddress: int
    Locals: Value array
}

type VmState = {
    mutable Pc: int
    Stack: Value array
    mutable Sp: int
    CallStack: StackFrame array
    mutable Csp: int
    Globals: Value array
    Constants: Value array
    Program: BytecodeProgram
    GoSubStack: int array
    mutable GoSubSp: int
    mutable ErrorMode: ErrorMode
    mutable LastErrorNumber: int
    mutable LastErrorDescription: string
}

let valueToInt = function
    | VInteger i -> i
    | VDouble d -> int d
    | VBoolean b -> if b then 1 else 0
    | VString s -> match System.Int32.TryParse(s) with true, i -> i | _ -> 0
    | _ -> 0

let valueToDouble = function
    | VInteger i -> float i
    | VDouble d -> d
    | VBoolean b -> if b then 1.0 else 0.0
    | VString s -> match System.Double.TryParse(s) with true, d -> d | _ -> 0.0
    | _ -> 0.0

let valueToString = function
    | VInteger i -> string i
    | VDouble d -> string d
    | VString s -> s
    | VBoolean b -> if b then "True" else "False"
    | VNull -> ""
    | VEmpty -> ""
    | VNothing -> ""
    | VHostObject _ -> ""
    | _ -> ""

let valueToBool = function
    | VBoolean b -> b
    | VInteger i -> i <> 0
    | VDouble d -> d <> 0.0
    | VString s -> s <> ""
    | VHostObject _ -> true
    | _ -> false

let inline numericBinOp intOp floatOp left right =
    match left, right with
    | VInteger l, VInteger r -> VInteger (intOp l r)
    | VDouble l, VDouble r -> VDouble (floatOp l r)
    | VInteger l, VDouble r -> VDouble (floatOp (float l) r)
    | VDouble l, VInteger r -> VDouble (floatOp l (float r))
    | _ ->
        // Coerce other types (VEmpty, VBoolean, etc.) to numbers
        let l = valueToInt left
        let r = valueToInt right
        VInteger (intOp l r)

let inline numericCmp (intOp: int -> int -> bool) (floatOp: float -> float -> bool) left right =
    match left, right with
    | VInteger l, VInteger r -> VBoolean (intOp l r)
    | VDouble l, VDouble r -> VBoolean (floatOp l r)
    | VInteger l, VDouble r -> VBoolean (floatOp (float l) r)
    | VDouble l, VInteger r -> VBoolean (floatOp l (float r))
    | _ -> VBoolean false

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
    let callStack = Array.zeroCreate 4096
    callStack.[0] <- {
        ReturnAddress = -1
        Locals = Array.create 256 VEmpty
    }
    {
        Pc = 0
        Stack = Array.create 65536 VEmpty
        Sp = 0
        CallStack = callStack
        Csp = 1
        Globals = Array.create (max 1 program.Globals) VEmpty
        Constants = constants
        Program = program
        GoSubStack = Array.zeroCreate 64
        GoSubSp = 0
        ErrorMode = NoHandler
        LastErrorNumber = 0
        LastErrorDescription = ""
    }

let inline push (state: VmState) (value: Value) =
    if state.Sp >= state.Stack.Length then failwith "Stack overflow"
    state.Stack.[state.Sp] <- value
    state.Sp <- state.Sp + 1

let inline pop (state: VmState) : Value =
    if state.Sp <= 0 then failwith "Stack underflow"
    state.Sp <- state.Sp - 1
    state.Stack.[state.Sp]

let inline peek (state: VmState) : Value =
    if state.Sp <= 0 then failwith "Stack underflow"
    state.Stack.[state.Sp - 1]

let inline pushFrame (state: VmState) (returnAddress: int) (localsCount: int) =
    if state.Csp >= state.CallStack.Length then failwith "Call stack overflow"
    state.CallStack.[state.Csp] <- {
        ReturnAddress = returnAddress
        Locals = Array.create (max localsCount 16) VEmpty
    }
    state.Csp <- state.Csp + 1

let inline popFrame (state: VmState) : StackFrame =
    if state.Csp <= 0 then failwith "Call stack underflow"
    state.Csp <- state.Csp - 1
    state.CallStack.[state.Csp]

let executeOneInstruction (state: VmState) =
    let instr = state.Program.Code.[state.Pc]

    match instr.Opcode with
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
        | v -> push state v
        state.Pc <- state.Pc + 1

    | StoreLocal slot ->
        let value = pop state
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | VRef (arr, idx) -> arr.[idx] <- value  // write-through ByRef
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
            | VString l, _ -> VString (l + valueToString right)
            | _, VString r -> VString (valueToString left + r)
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
        let r = valueToDouble right
        if r = 0.0 then failwith "Division by zero"
        push state (VDouble (valueToDouble left / r))
        state.Pc <- state.Pc + 1

    | IntDivide ->
        let right = pop state
        let left = pop state
        let r = valueToInt right
        if r = 0 then failwith "Division by zero"
        push state (VInteger (valueToInt left / r))
        state.Pc <- state.Pc + 1

    | Modulus ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VInteger l, VInteger r when r <> 0 -> VInteger (l % r)
            | _ -> VInteger 0
        push state result
        state.Pc <- state.Pc + 1

    | Power ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
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
        push state (VString (valueToString left + valueToString right))
        state.Pc <- state.Pc + 1

    | Equal ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VInteger _, VDouble _ | VDouble _, VInteger _ ->
                valueToDouble left = valueToDouble right
            | _ -> left = right
        push state (VBoolean result)
        state.Pc <- state.Pc + 1

    | NotEqual ->
        let right = pop state
        let left = pop state
        let result =
            match left, right with
            | VInteger _, VDouble _ | VDouble _, VInteger _ ->
                valueToDouble left <> valueToDouble right
            | _ -> left <> right
        push state (VBoolean result)
        state.Pc <- state.Pc + 1

    | LessThan ->
        let right = pop state
        let left = pop state
        push state (numericCmp (<) (<) left right)
        state.Pc <- state.Pc + 1

    | LessEqual ->
        let right = pop state
        let left = pop state
        push state (numericCmp (<=) (<=) left right)
        state.Pc <- state.Pc + 1

    | GreaterThan ->
        let right = pop state
        let left = pop state
        push state (numericCmp (>) (>) left right)
        state.Pc <- state.Pc + 1

    | GreaterEqual ->
        let right = pop state
        let left = pop state
        push state (numericCmp (>=) (>=) left right)
        state.Pc <- state.Pc + 1

    | And ->
        let right = pop state
        let left = pop state
        push state (VBoolean (valueToBool left && valueToBool right))
        state.Pc <- state.Pc + 1

    | Or ->
        let right = pop state
        let left = pop state
        push state (VBoolean (valueToBool left || valueToBool right))
        state.Pc <- state.Pc + 1

    | Xor ->
        let right = pop state
        let left = pop state
        push state (VBoolean (valueToBool left <> valueToBool right))
        state.Pc <- state.Pc + 1

    | Eqv ->
        let right = pop state
        let left = pop state
        push state (VBoolean (valueToBool left = valueToBool right))
        state.Pc <- state.Pc + 1

    | Imp ->
        let right = pop state
        let left = pop state
        push state (VBoolean (not (valueToBool left) || valueToBool right))
        state.Pc <- state.Pc + 1

    | Not ->
        let value = pop state
        push state (VBoolean (not (valueToBool value)))
        state.Pc <- state.Pc + 1

    | Negate ->
        let value = pop state
        let result =
            match value with
            | VInteger i -> VInteger (-i)
            | VDouble d -> VDouble (-d)
            | _ -> VInteger 0
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
    | ArrayNew ->
        let size = valueToInt (pop state)
        push state (VArray (Array.create (max 0 size) VEmpty))
        state.Pc <- state.Pc + 1

    | ArrayGet ->
        let index = valueToInt (pop state)
        let arr = pop state
        match arr with
        | VArray a ->
            if index >= 0 && index < a.Length then push state a.[index]
            else push state VEmpty
        | VString s ->
            if index >= 0 && index < s.Length then push state (VString (string s.[index]))
            else push state VEmpty
        | _ -> push state VEmpty
        state.Pc <- state.Pc + 1

    | ArraySet ->
        let value = pop state
        let index = valueToInt (pop state)
        let arr = pop state
        match arr with
        | VArray a ->
            if index >= 0 && index < a.Length then
                a.[index] <- value
        | _ -> ()
        state.Pc <- state.Pc + 1

    | ArrayLength ->
        let arr = pop state
        match arr with
        | VArray a -> push state (VInteger a.Length)
        | VString s -> push state (VInteger s.Length)
        | _ -> push state (VInteger 0)
        state.Pc <- state.Pc + 1

    | ReDimPreserve ->
        let newSize = valueToInt (pop state)
        let oldArr = pop state
        match oldArr with
        | VArray old ->
            let newArr = Array.create (max 0 newSize) VEmpty
            let copyLen = min old.Length newArr.Length
            System.Array.Copy(old, newArr, copyLen)
            push state (VArray newArr)
        | _ ->
            push state (VArray (Array.create (max 0 newSize) VEmpty))
        state.Pc <- state.Pc + 1

    // Objects
    | NewObj classIdx ->
        let classDef = state.Program.Classes.[classIdx]
        let fields = Dictionary<string, Value>()
        for fieldName in classDef.Fields do
            fields.[fieldName] <- VEmpty
        let obj = VObject { ClassName = classDef.Name; Fields = fields; ClassIndex = classIdx }
        // Auto-call Class_Initialize if it exists
        match Map.tryFind "Class_Initialize" classDef.Methods with
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

    | GetMember name ->
        let obj = pop state
        match obj with
        | VObject vbObj ->
            match vbObj.Fields.TryGetValue(name) with
            | true, value ->
                push state value
                state.Pc <- state.Pc + 1
            | false, _ ->
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind name classDef.PropertyGetters with
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

    | SetMember name ->
        let value = pop state
        let obj = pop state
        match obj with
        | VObject vbObj ->
            if vbObj.Fields.ContainsKey(name) then
                vbObj.Fields.[name] <- value
                state.Pc <- state.Pc + 1
            else
                let classDef = state.Program.Classes.[vbObj.ClassIndex]
                match Map.tryFind name classDef.PropertyLetters with
                | Some funcIdx ->
                    let funcDef = state.Program.Functions.[funcIdx]
                    pushFrame state (state.Pc + 1) funcDef.LocalsCount
                    let frame = state.CallStack.[state.Csp - 1]
                    frame.Locals.[0] <- VObject vbObj
                    frame.Locals.[1] <- value
                    state.Pc <- funcDef.StartPC
                | None ->
                    state.Pc <- state.Pc + 1
        | VHostObject hostObj ->
            ignore (hostObj.SetMember name value)
            state.Pc <- state.Pc + 1
        | _ ->
            state.Pc <- state.Pc + 1

    | CallMethod (name, argCount) ->
        let args = Array.zeroCreate argCount
        for i in 0 .. argCount - 1 do
            args.[i] <- pop state
        let obj = pop state
        match obj with
        | VObject vbObj ->
            let classDef = state.Program.Classes.[vbObj.ClassIndex]
            match Map.tryFind name classDef.Methods with
            | Some funcIdx ->
                let funcDef = state.Program.Functions.[funcIdx]
                pushFrame state (state.Pc + 1) funcDef.LocalsCount
                let frame = state.CallStack.[state.Csp - 1]
                frame.Locals.[0] <- VObject vbObj  // Me
                for i in 0 .. argCount - 1 do
                    frame.Locals.[i + 1] <- args.[i]
                state.Pc <- funcDef.StartPC
            | None ->
                // Try property getter with args (default property)
                push state VEmpty
                state.Pc <- state.Pc + 1
        | VHostObject hostObj ->
            hostObj.CallMethod name args
            |> Option.defaultValue VEmpty
            |> push state
            state.Pc <- state.Pc + 1
        | _ ->
            push state VEmpty
            state.Pc <- state.Pc + 1

    | CallDefault argCount ->
        let args = Array.zeroCreate argCount
        for i in 0 .. argCount - 1 do
            args.[i] <- pop state
        let obj = pop state
        let arrayFallback () =
            if argCount = 1 then
                let index = valueToInt args.[0]
                match obj with
                | VArray a when index >= 0 && index < a.Length -> a.[index]
                | VString s when index >= 0 && index < s.Length -> VString (string s.[index])
                | _ -> VEmpty
            else
                VEmpty
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
        let result = Builtins.callBuiltin name args
        push state result
        state.Pc <- state.Pc + 1

    // Control flow
    | Jump target ->
        state.Pc <- target

    | JumpIfFalse target ->
        let value = pop state
        if not (valueToBool value) then state.Pc <- target
        else state.Pc <- state.Pc + 1

    | JumpIfTrue target ->
        let value = pop state
        if valueToBool value then state.Pc <- target
        else state.Pc <- state.Pc + 1

    | Call (funcIndex, argCount) ->
        let funcDef = state.Program.Functions.[funcIndex]
        let args = Array.zeroCreate argCount
        for i in 0 .. argCount - 1 do
            args.[i] <- pop state

        pushFrame state (state.Pc + 1) funcDef.LocalsCount
        let frame = state.CallStack.[state.Csp - 1]
        for i in 0 .. argCount - 1 do
            frame.Locals.[i] <- args.[i]

        state.Pc <- funcDef.StartPC

    | Return ->
        let retVal = pop state
        let frame = popFrame state
        push state retVal
        state.Pc <- frame.ReturnAddress

    | Bytecode.GoSub target ->
        if state.GoSubSp >= state.GoSubStack.Length then failwith "GoSub stack overflow"
        state.GoSubStack.[state.GoSubSp] <- state.Pc + 1
        state.GoSubSp <- state.GoSubSp + 1
        state.Pc <- target

    | ReturnSub ->
        if state.GoSubSp <= 0 then failwith "Return without GoSub"
        state.GoSubSp <- state.GoSubSp - 1
        state.Pc <- state.GoSubStack.[state.GoSubSp]

    | OnErrorResumeNext ->
        state.ErrorMode <- ResumeNextMode
        state.Pc <- state.Pc + 1

    | OnErrorGoToZero ->
        state.ErrorMode <- NoHandler
        state.Pc <- state.Pc + 1

    | OnErrorGoToLabel target ->
        state.ErrorMode <- GoToLabelMode target
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
        state.LastErrorNumber <- num
        state.LastErrorDescription <- desc
        push state VEmpty
        match state.ErrorMode with
        | NoHandler -> failwithf "Runtime error %d: %s" num desc
        | _ -> ()
        state.Pc <- state.Pc + 1

    // ByRef
    | MakeRefLocal slot ->
        let frame = state.CallStack.[state.Csp - 1]
        match frame.Locals.[slot] with
        | VRef _ as r -> push state r  // forward existing ref
        | _ -> push state (VRef (frame.Locals, slot))
        state.Pc <- state.Pc + 1

    | MakeRefGlobal slot ->
        push state (VRef (state.Globals, slot))
        state.Pc <- state.Pc + 1

    | Halt ->
        state.Pc <- state.Program.Code.Length

let run (state: VmState) =
    while state.Pc < state.Program.Code.Length do
        match state.ErrorMode with
        | NoHandler ->
            executeOneInstruction state
        | ResumeNextMode ->
            let savedSp = state.Sp
            try
                executeOneInstruction state
            with ex ->
                state.Sp <- savedSp  // restore stack on error
                state.LastErrorNumber <- 11
                state.LastErrorDescription <- ex.Message
                state.Pc <- state.Pc + 1
        | GoToLabelMode target ->
            let savedSp = state.Sp
            try
                executeOneInstruction state
            with ex ->
                state.Sp <- savedSp
                state.LastErrorNumber <- 11
                state.LastErrorDescription <- ex.Message
                state.Pc <- target

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
    let state = emptyState program.Constants program
    setInitialGlobals state globals
    run state
    state

let execute (program: BytecodeProgram) : VmState =
    executeWithGlobals Seq.empty program
