module Rbt.Vb.Bytecode

open Values

type Opcode =
    | Nop
    | Statement of int
    | LoadConst of int
    | LoadNull
    | LoadEmpty
    | LoadTrue
    | LoadFalse
    | LoadLocal of int
    | StoreLocal of int
    | LoadGlobal of int
    | StoreGlobal of int
    | Pop
    | Dup
    // Arithmetic
    | Add
    | Subtract
    | Multiply
    | Divide
    | IntDivide
    | Modulus
    | Power
    | Concat
    // Comparison
    | Equal
    | NotEqual
    | LessThan
    | LessEqual
    | GreaterThan
    | GreaterEqual
    // Logical
    | And
    | Or
    | Xor
    | Eqv
    | Imp
    | Not
    | Negate
    | IsOp
    | LikeOp
    // Arrays
    | ArrayNew of int
    | ArrayGet of int
    | ArrayGetFlat
    | MakeRefArray of int
    | ArraySet of int
    | ArrayLength
    | ReDimPreserve of int
    // Objects
    | NewObj of int           // class index
    | GetMember of string     // member name
    | MakeRefMember of string // field location, or a value for properties/hosts
    | SetMember of string * bool     // member name
    | CallMethod of string * int  // method name, arg count
    | CallDefault of int      // arg count; host default call or array indexing
    | TypeCheck of string     // class name for TypeOf...Is
    // Control flow
    | CallBuiltin of string * int
    | Jump of int
    | JumpIfFalse of int
    | JumpIfTrue of int
    | Call of int * int
    | Return
    | GoSub of int
    | ReturnSub
    // Error handling
    | OnErrorResumeNext
    | OnErrorGoToZero
    | OnErrorGoToLabel of int
    // Err object
    | LoadErrNumber
    | LoadErrDescription
    | ClearErr
    | RaiseErr
    // ByRef
    | MakeRefLocal of int
    | MakeRefGlobal of int
    | Halt
    // 0.3.0
    | Coerce of int               // ValueSemantics.DeclaredType code
    | LetValue                    // Let-coercion: read a host object's default property
    | ForEachPrepare              // collection -> array snapshot
    | Resume of int               // 0 = Resume (retry), 1 = Resume Next
    | ResumeLabel of int
    | OnErrorGoToMinusOne
    | LoadErrObject
    | LoadErrSource
    | SetDefault                  // obj = value through the default member
    | CopyUdt                     // copy a user-defined type value
    | ArrayFill of int            // fill Empty elements with constant
    | ArrayFillNew of int         // fill Empty elements with new instances of a class
    | CallMethodNamed of string * string array  // method, argument names ("" = positional)

type Instruction = {
    Opcode: Opcode
    LineNumber: int
}

let makeInstruction opcode lineNumber =
    { Opcode = opcode; LineNumber = lineNumber }

type FunctionDef = {
    Name: string
    ParamsCount: int
    LocalsCount: int
    ByRefParams: bool array
    Code: Instruction array
    StartPC: int
    Module: string
    ParamNames: string array
    /// Initial local slots (typed defaults), or empty for all-Empty locals.
    InitialLocals: Value array
}

type ClassDef = {
    Name: string
    Fields: string array
    Methods: Map<string, int>          // method name -> FunctionDef index
    PropertyGetters: Map<string, int>
    PropertyLetters: Map<string, int>
    PropertySetters: Map<string, int>
    IsValueType: bool
    /// Per field: 0 Variant, > 0 a ValueSemantics.DeclaredType code, < 0 a
    /// nested user-defined type -(classIndex + 1).
    FieldTypes: int array
}

type BytecodeProgram = {
    TextCompare: bool
    Code: Instruction array
    Constants: Value array
    Globals: int
    GlobalNames: string array
    Functions: FunctionDef array
    Classes: ClassDef array
    /// Start pc of each module's module-level code in the main region, in order.
    ModuleStarts: (int * string) array
}

// Structural validation protects the runtime from malformed bytecode; it is not a sandbox.
let validate (program: BytecodeProgram) =
    let check condition message = if not condition then failwith ("Invalid bytecode: " + message)
    check (not (isNull program.Code) && program.Code.Length > 0 && program.Code.Length <= 1000000) "code size"
    check (not (isNull program.Constants) && program.Constants.Length <= 1000000) "constants"
    check (program.Globals >= 0 && program.Globals <= 1000000 && not (isNull program.GlobalNames) && program.GlobalNames.Length = program.Globals) "globals"
    check (not (isNull program.Functions) && not (isNull program.Classes)) "declarations"
    let validTarget target = target >= 0 && target < program.Code.Length
    for constant in program.Constants do
        match constant with
        | VInteger _ | VDouble _ | VString _ | VBoolean _ | VNull | VEmpty | VNothing | VUndefined -> ()
        | _ -> failwith "Invalid bytecode: runtime value in constant pool"
    for f in program.Functions do
        check (f.ParamsCount >= 0 && f.LocalsCount > f.ParamsCount && f.LocalsCount <= 1000000) "function locals"
        check (not (isNull f.ByRefParams) && f.ByRefParams.Length = f.ParamsCount && validTarget f.StartPC) "function metadata"
        check (not (isNull f.InitialLocals) && (f.InitialLocals.Length = 0 || f.InitialLocals.Length = f.LocalsCount)) "function initial locals"
        for v in f.InitialLocals do
            match v with
            | VInteger _ | VDouble _ | VString _ | VBoolean _ | VEmpty | VNothing -> ()
            | _ -> failwith "Invalid bytecode: runtime value in initial locals"
    for c in program.Classes do
        check (not (isNull c.FieldTypes) && c.FieldTypes.Length <= c.Fields.Length) "class fields"
        for code in c.FieldTypes do check (code >= -program.Classes.Length && code <= 64) "field type"
        for m in [c.Methods; c.PropertyGetters; c.PropertyLetters; c.PropertySetters] do
            for KeyValue(_, index) in m do
                check (index >= 0 && index < program.Functions.Length) "class member"
    for instruction in program.Code do
        match instruction.Opcode with
        | LoadConst i -> check (i >= 0 && i < program.Constants.Length) "constant slot"
        | LoadGlobal i | StoreGlobal i | MakeRefGlobal i -> check (i >= 0 && i < program.Globals) "global slot"
        | LoadLocal i | StoreLocal i | MakeRefLocal i -> check (i >= 0 && i < 1000000) "local slot"
        | Jump i | JumpIfFalse i | JumpIfTrue i | GoSub i | OnErrorGoToLabel i | ResumeLabel i | Statement i -> check (validTarget i) "target"
        | ArrayFill i -> check (i >= 0 && i < program.Constants.Length) "constant slot"
        | ArrayFillNew i -> check (i >= 0 && i < program.Classes.Length) "class"
        | Call (i, n) -> check (i >= 0 && i < program.Functions.Length && n >= 0 && n <= program.Functions.[i].ParamsCount) "call"
        | NewObj i -> check (i >= 0 && i < program.Classes.Length) "class"
        | ArrayNew n | ArrayGet n | ArraySet n | ReDimPreserve n | MakeRefArray n -> check (n > 0 && n <= 32) "array rank"
        | CallMethodNamed (_, names) -> check (not (isNull names) && names.Length <= 1024) "argument count"
        | CallMethod (_, n) | CallDefault n | CallBuiltin (_, n) -> check (n >= 0 && n <= 1024) "argument count"
        | _ -> ()

    let starts = program.Functions |> Array.map (fun f -> f.StartPC) |> Array.sort
    for i in 1 .. starts.Length - 1 do check (starts.[i] <> starts.[i - 1]) "duplicate function entry"
    let mainStart =
        match program.Code.[0].Opcode with
        | Jump target when starts.Length > 0 -> target
        | _ -> 0
    for f in program.Functions do
        check (f.StartPC > 0 && f.StartPC < mainStart && f.Code.Length = 0) "function region"
    for c in program.Classes do
        for m in [c.Methods; c.PropertyGetters; c.PropertyLetters; c.PropertySetters] do
            for KeyValue(_, index) in m do
                let f = program.Functions.[index]
                check (f.LocalsCount > f.ParamsCount + 1) "method receiver"
    let byStart = program.Functions |> Array.map (fun f -> f.StartPC, f) |> Map.ofArray
    let mutable current = None
    for pc in 0 .. program.Code.Length - 1 do
        if pc = mainStart then current <- None
        match Map.tryFind pc byStart with Some f -> current <- Some f | None -> ()
        match current, program.Code.[pc].Opcode with
        | Some f, (LoadLocal slot | StoreLocal slot | MakeRefLocal slot) ->
            check (slot < f.LocalsCount) "function local slot"
        | _ -> ()

    let regions = Array.create program.Code.Length -1
    let mutable regionIndex = -2
    let mutable nextStart = 0
    for pc in 0 .. mainStart - 1 do
        if nextStart < starts.Length && pc = starts.[nextStart] then
            regionIndex <- nextStart
            nextStart <- nextStart + 1
        regions.[pc] <- regionIndex
    for pc in 0 .. program.Code.Length - 1 do
        match program.Code.[pc].Opcode with
        | Jump target when pc = 0 && starts.Length > 0 -> check (target = mainStart) "entry point"
        | Jump target | JumpIfFalse target | JumpIfTrue target | GoSub target | OnErrorGoToLabel target | ResumeLabel target | Statement target ->
            check (regions.[pc] = regions.[target]) "cross-region target"
        | _ -> ()
