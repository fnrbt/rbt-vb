module Rbt.Vb.BytecodeIO

// Binary (de)serialization of a compiled BytecodeProgram — our analogue of
// VBA's persisted p-code. Lets a compiled macro be saved and re-run without
// re-parsing/compiling the source. Native-only (uses System.IO); the Fable
// build never persists bytecode to disk, so it gets throwing stubs.

open Values
open Bytecode

let magic = 0x46_56_42_43   // "FVBC"
let version = 4

#if FABLE_COMPILER
let serialize (_: BytecodeProgram) : byte[] = failwith "bytecode serialization is native-only"
let deserialize (_: byte[]) : BytecodeProgram = failwith "bytecode serialization is native-only"
#else
open System.IO

let private readString (r: BinaryReader) =
    let mutable length = 0
    let mutable shift = 0
    let mutable more = true
    while more do
        if shift >= 35 then failwith "Invalid string length"
        let b = int (r.ReadByte())
        if shift = 28 && b > 7 then failwith "Invalid string length"
        length <- length ||| ((b &&& 127) <<< shift)
        more <- (b &&& 128) <> 0
        shift <- shift + 7
    if length < 0 || length > 1000000 || int64 length > r.BaseStream.Length - r.BaseStream.Position then
        failwith "Invalid string length"
    let bytes = r.ReadBytes length
    System.Text.Encoding.UTF8.GetString bytes

// --- Value (constant-pool literals only; runtime-only cases never appear) ---
let private writeValue (w: BinaryWriter) (v: Value) =
    match v with
    | VInteger n -> w.Write 0uy; w.Write n
    | VDouble d -> w.Write 1uy; w.Write d
    | VString s -> w.Write 2uy; w.Write s
    | VBoolean b -> w.Write 3uy; w.Write b
    | VNull -> w.Write 4uy
    | VEmpty -> w.Write 5uy
    | VNothing -> w.Write 6uy
    | VUndefined -> w.Write 7uy
    | VDate d -> w.Write 8uy; w.Write d
    | VCurrency c -> w.Write 9uy; w.Write c
    | VError e -> w.Write 10uy; w.Write e
    | other -> failwithf "non-constant value in bytecode: %A" other

let private readValue (r: BinaryReader) : Value =
    match r.ReadByte() with
    | 0uy -> VInteger (r.ReadInt32())
    | 1uy -> VDouble (r.ReadDouble())
    | 2uy -> VString (readString r)
    | 3uy -> VBoolean (r.ReadBoolean())
    | 4uy -> VNull
    | 5uy -> VEmpty
    | 6uy -> VNothing
    | 7uy -> VUndefined
    | 8uy -> VDate (r.ReadDouble())
    | 9uy -> VCurrency (r.ReadInt64())
    | 10uy -> VError (r.ReadInt32())
    | t -> failwithf "bad value tag %d" (int t)

// --- Opcode (tag byte + payload) ---
let private writeOpcode (w: BinaryWriter) (op: Opcode) =
    let t (b: int) = w.Write (byte b)
    match op with
    | Statement i -> t 64; w.Write i
    | ArrayGetFlat -> t 65
    | MakeRefArray n -> t 66; w.Write n
    | MakeRefMember s -> t 67; w.Write s
    | Nop -> t 0
    | LoadConst i -> t 1; w.Write i
    | LoadNull -> t 2
    | LoadEmpty -> t 3
    | LoadTrue -> t 4
    | LoadFalse -> t 5
    | LoadLocal i -> t 6; w.Write i
    | StoreLocal i -> t 7; w.Write i
    | LoadGlobal i -> t 8; w.Write i
    | StoreGlobal i -> t 9; w.Write i
    | Pop -> t 10
    | Dup -> t 11
    | Add -> t 12
    | Subtract -> t 13
    | Multiply -> t 14
    | Divide -> t 15
    | IntDivide -> t 16
    | Modulus -> t 17
    | Power -> t 18
    | Concat -> t 19
    | Equal -> t 20
    | NotEqual -> t 21
    | LessThan -> t 22
    | LessEqual -> t 23
    | GreaterThan -> t 24
    | GreaterEqual -> t 25
    | And -> t 26
    | Or -> t 27
    | Xor -> t 28
    | Eqv -> t 29
    | Imp -> t 30
    | Not -> t 31
    | Negate -> t 32
    | IsOp -> t 33
    | LikeOp -> t 34
    | ArrayNew n -> t 35; w.Write n
    | ArrayGet n -> t 36; w.Write n
    | ArraySet n -> t 37; w.Write n
    | ArrayLength -> t 38
    | ReDimPreserve n -> t 39; w.Write n
    | NewObj i -> t 40; w.Write i
    | GetMember s -> t 41; w.Write s
    | SetMember (s, isSet) -> t 42; w.Write s; w.Write isSet
    | CallMethod (s, n) -> t 43; w.Write s; w.Write n
    | CallDefault n -> t 44; w.Write n
    | TypeCheck s -> t 45; w.Write s
    | CallBuiltin (s, n) -> t 46; w.Write s; w.Write n
    | Jump i -> t 47; w.Write i
    | JumpIfFalse i -> t 48; w.Write i
    | JumpIfTrue i -> t 49; w.Write i
    | Call (a, b) -> t 50; w.Write a; w.Write b
    | Return -> t 51
    | GoSub i -> t 52; w.Write i
    | ReturnSub -> t 53
    | OnErrorResumeNext -> t 54
    | OnErrorGoToZero -> t 55
    | OnErrorGoToLabel i -> t 56; w.Write i
    | LoadErrNumber -> t 57
    | LoadErrDescription -> t 58
    | ClearErr -> t 59
    | RaiseErr -> t 60
    | MakeRefLocal i -> t 61; w.Write i
    | MakeRefGlobal i -> t 62; w.Write i
    | Halt -> t 63
    | Coerce n -> t 68; w.Write n
    | LetValue -> t 69
    | ForEachPrepare -> t 70
    | Resume n -> t 71; w.Write n
    | ResumeLabel n -> t 72; w.Write n
    | OnErrorGoToMinusOne -> t 73
    | LoadErrObject -> t 74
    | LoadErrSource -> t 75
    | SetDefault -> t 76
    | CopyUdt -> t 77
    | ArrayFill n -> t 78; w.Write n
    | ArrayFillNew n -> t 79; w.Write n
    | CallMethodNamed (name, names) -> t 80; w.Write name; w.Write names.Length; (for n in names do w.Write n)
    | LineLabel n -> t 81; w.Write n
    | LoadErl -> t 82
    | StopOp -> t 83
    | EndOp -> t 84
    | LetLocal n -> t 85; w.Write n
    | LetGlobal n -> t 86; w.Write n
    | SetMemberIndexed (name, n) -> t 87; w.Write name; w.Write n
    | ArrayReset n -> t 88; w.Write n
    | EventHandlers name -> t 89; w.Write name
    | CallHandler n -> t 90; w.Write n
    | BindEvents n -> t 91; w.Write n
    | ReleaseCheck -> t 92
    | CallByNameOp n -> t 93; w.Write n

let private readOpcode (r: BinaryReader) : Opcode =
    let i () = r.ReadInt32()
    let s () = readString r
    match int (r.ReadByte()) with
    | 0 -> Nop
    | 1 -> LoadConst (i())
    | 2 -> LoadNull
    | 3 -> LoadEmpty
    | 4 -> LoadTrue
    | 5 -> LoadFalse
    | 6 -> LoadLocal (i())
    | 7 -> StoreLocal (i())
    | 8 -> LoadGlobal (i())
    | 9 -> StoreGlobal (i())
    | 10 -> Pop
    | 11 -> Dup
    | 12 -> Add
    | 13 -> Subtract
    | 14 -> Multiply
    | 15 -> Divide
    | 16 -> IntDivide
    | 17 -> Modulus
    | 18 -> Power
    | 19 -> Concat
    | 20 -> Equal
    | 21 -> NotEqual
    | 22 -> LessThan
    | 23 -> LessEqual
    | 24 -> GreaterThan
    | 25 -> GreaterEqual
    | 26 -> And
    | 27 -> Or
    | 28 -> Xor
    | 29 -> Eqv
    | 30 -> Imp
    | 31 -> Not
    | 32 -> Negate
    | 33 -> IsOp
    | 34 -> LikeOp
    | 35 -> ArrayNew (i())
    | 36 -> ArrayGet (i())
    | 37 -> ArraySet (i())
    | 38 -> ArrayLength
    | 39 -> ReDimPreserve (i())
    | 40 -> NewObj (i())
    | 41 -> GetMember (s())
    | 42 -> let name = s() in SetMember (name, r.ReadBoolean())
    | 43 -> let a = s() in CallMethod (a, i())
    | 44 -> CallDefault (i())
    | 45 -> TypeCheck (s())
    | 46 -> let a = s() in CallBuiltin (a, i())
    | 47 -> Jump (i())
    | 48 -> JumpIfFalse (i())
    | 49 -> JumpIfTrue (i())
    | 50 -> let a = i() in Call (a, i())
    | 51 -> Return
    | 52 -> GoSub (i())
    | 53 -> ReturnSub
    | 54 -> OnErrorResumeNext
    | 55 -> OnErrorGoToZero
    | 56 -> OnErrorGoToLabel (i())
    | 57 -> LoadErrNumber
    | 58 -> LoadErrDescription
    | 59 -> ClearErr
    | 60 -> RaiseErr
    | 61 -> MakeRefLocal (i())
    | 62 -> MakeRefGlobal (i())
    | 63 -> Halt
    | 64 -> Statement (i())
    | 65 -> ArrayGetFlat
    | 66 -> MakeRefArray (i())
    | 67 -> MakeRefMember (s())
    | 68 -> Coerce (i())
    | 69 -> LetValue
    | 70 -> ForEachPrepare
    | 71 -> Resume (i())
    | 72 -> ResumeLabel (i())
    | 73 -> OnErrorGoToMinusOne
    | 74 -> LoadErrObject
    | 75 -> LoadErrSource
    | 76 -> SetDefault
    | 77 -> CopyUdt
    | 78 -> ArrayFill (i())
    | 79 -> ArrayFillNew (i())
    | 80 ->
        let name = s()
        let count = i()
        if count < 0 || count > 1024 then failwith "Invalid argument count"
        CallMethodNamed (name, Array.init count (fun _ -> s()))
    | 81 -> LineLabel (i())
    | 82 -> LoadErl
    | 83 -> StopOp
    | 84 -> EndOp
    | 85 -> LetLocal (i())
    | 86 -> LetGlobal (i())
    | 87 -> let name = s() in SetMemberIndexed (name, i())
    | 88 -> ArrayReset (i())
    | 89 -> EventHandlers (s())
    | 90 -> CallHandler (i())
    | 91 -> BindEvents (i())
    | 92 -> ReleaseCheck
    | 93 -> CallByNameOp (i())
    | t -> failwithf "bad opcode tag %d" t

let private writeArray (w: BinaryWriter) (writeItem: 'a -> unit) (xs: 'a array) =
    w.Write xs.Length
    for x in xs do writeItem x

let private readArray (r: BinaryReader) (readItem: unit -> 'a) : 'a array =
    let n = r.ReadInt32()
    if n < 0 || n > 1000000 || int64 n > r.BaseStream.Length - r.BaseStream.Position then failwith "Invalid bytecode count"
    Array.init n (fun _ -> readItem ())

let private writeInstr (w: BinaryWriter) (ins: Instruction) =
    writeOpcode w ins.Opcode
    w.Write ins.LineNumber
let private readInstr (r: BinaryReader) : Instruction =
    let op = readOpcode r
    { Opcode = op; LineNumber = r.ReadInt32() }

let private writeMap (w: BinaryWriter) (m: Map<string, int>) =
    w.Write (Map.count m)
    for KeyValue(k, v) in m do w.Write k; w.Write v
let private readMap (r: BinaryReader) : Map<string, int> =
    let n = r.ReadInt32()
    if n < 0 || n > 1000000 || int64 n > r.BaseStream.Length - r.BaseStream.Position then failwith "Invalid bytecode count"
    Seq.init n (fun _ -> let k = readString r in k, r.ReadInt32()) |> Map.ofSeq

let serialize (prog: BytecodeProgram) : byte[] =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    w.Write magic
    w.Write version
    w.Write prog.TextCompare
    writeArray w (writeInstr w) prog.Code
    writeArray w (writeValue w) prog.Constants
    w.Write prog.Globals
    writeArray w (fun (s: string) -> w.Write s) prog.GlobalNames
    writeArray w (fun (f: FunctionDef) ->
        w.Write f.Name
        w.Write f.ParamsCount
        w.Write f.LocalsCount
        writeArray w (fun (b: bool) -> w.Write b) f.ByRefParams
        writeArray w (writeInstr w) f.Code
        w.Write f.StartPC
        w.Write f.Module
        writeArray w (fun (n: string) -> w.Write n) f.ParamNames
        writeArray w (writeValue w) f.InitialLocals) prog.Functions
    writeArray w (fun (c: ClassDef) ->
        w.Write c.Name
        writeArray w (fun (s: string) -> w.Write s) c.Fields
        writeMap w c.Methods
        writeMap w c.PropertyGetters
        writeMap w c.PropertyLetters
        writeMap w c.PropertySetters
        w.Write c.IsValueType
        writeArray w (fun (code: int) -> w.Write code) c.FieldTypes
        w.Write c.DefaultMember
        writeArray w (fun (s: string) -> w.Write s) c.Implements) prog.Classes
    writeArray w (fun ((pc: int), (name: string)) -> w.Write pc; w.Write name) prog.ModuleStarts
    writeArray w (fun (b: EventBinding) -> writeMap w b.Handlers; w.Write b.ClassSink) prog.EventBindings
    w.Flush()
    ms.ToArray()

let deserialize (bytes: byte[]) : BytecodeProgram =
    use ms = new MemoryStream(bytes)
    use r = new BinaryReader(ms)
    let m = r.ReadInt32()
    let v = r.ReadInt32()
    if m <> magic then failwithf "not a bytecode blob (magic %08x)" m
    if v <> version then failwithf "unsupported bytecode version %d" v
    let textCompare = r.ReadBoolean()
    let code = readArray r (fun () -> readInstr r)
    let constants = readArray r (fun () -> readValue r)
    let globals = r.ReadInt32()
    let globalNames = readArray r (fun () -> readString r)
    let functions =
        readArray r (fun () ->
            let name = readString r
            let p = r.ReadInt32()
            let l = r.ReadInt32()
            let byref = readArray r (fun () -> r.ReadBoolean())
            let fcode = readArray r (fun () -> readInstr r)
            let start = r.ReadInt32()
            let moduleName = readString r
            let paramNames = readArray r (fun () -> readString r)
            let initialLocals = readArray r (fun () -> readValue r)
            { Name = name; ParamsCount = p; LocalsCount = l; ByRefParams = byref; Code = fcode; StartPC = start; Module = moduleName; ParamNames = paramNames; InitialLocals = initialLocals })
    let classes =
        readArray r (fun () ->
            let name = readString r
            let fields = readArray r (fun () -> readString r)
            let methods = readMap r
            let getters = readMap r
            let letters = readMap r
            let setters = readMap r
            let isValueType = r.ReadBoolean()
            let fieldTypes = readArray r (fun () -> r.ReadInt32())
            let defaultMember = readString r
            let implements = readArray r (fun () -> readString r)
            { Name = name; Fields = fields; Methods = methods
              PropertyGetters = getters; PropertyLetters = letters; PropertySetters = setters; IsValueType = isValueType; FieldTypes = fieldTypes
              DefaultMember = defaultMember; Implements = implements })
    let moduleStarts = readArray r (fun () -> let pc = r.ReadInt32() in pc, readString r)
    let bindings = readArray r (fun () -> let handlers = readMap r in { Handlers = handlers; ClassSink = r.ReadBoolean() })
    if ms.Position <> ms.Length then failwith "Trailing bytecode data"
    let program = { TextCompare = textCompare; Code = code; Constants = constants; Globals = globals; GlobalNames = globalNames
                    Functions = functions; Classes = classes; ModuleStarts = moduleStarts; EventBindings = bindings }
    validate program
    program
#endif
