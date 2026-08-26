module Rbt.Vb.BytecodeIO

// Binary (de)serialization of a compiled BytecodeProgram — our analogue of
// VBA's persisted p-code. Lets a compiled macro be saved and re-run without
// re-parsing/compiling the source. Native-only (uses System.IO); the Fable
// build never persists bytecode to disk, so it gets throwing stubs.

open Values
open Bytecode

let magic = 0x46_56_42_43   // "FVBC"
let version = 1

#if FABLE_COMPILER
let serialize (_: BytecodeProgram) : byte[] = failwith "bytecode serialization is native-only"
let deserialize (_: byte[]) : BytecodeProgram = failwith "bytecode serialization is native-only"
#else
open System.IO

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
    | other -> failwithf "non-constant value in bytecode: %A" other

let private readValue (r: BinaryReader) : Value =
    match r.ReadByte() with
    | 0uy -> VInteger (r.ReadInt32())
    | 1uy -> VDouble (r.ReadDouble())
    | 2uy -> VString (r.ReadString())
    | 3uy -> VBoolean (r.ReadBoolean())
    | 4uy -> VNull
    | 5uy -> VEmpty
    | 6uy -> VNothing
    | 7uy -> VUndefined
    | t -> failwithf "bad value tag %d" (int t)

// --- Opcode (tag byte + payload) ---
let private writeOpcode (w: BinaryWriter) (op: Opcode) =
    let t (b: int) = w.Write (byte b)
    match op with
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
    | ArrayNew -> t 35
    | ArrayGet -> t 36
    | ArraySet -> t 37
    | ArrayLength -> t 38
    | ReDimPreserve -> t 39
    | NewObj i -> t 40; w.Write i
    | GetMember s -> t 41; w.Write s
    | SetMember s -> t 42; w.Write s
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

let private readOpcode (r: BinaryReader) : Opcode =
    let i () = r.ReadInt32()
    let s () = r.ReadString()
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
    | 35 -> ArrayNew
    | 36 -> ArrayGet
    | 37 -> ArraySet
    | 38 -> ArrayLength
    | 39 -> ReDimPreserve
    | 40 -> NewObj (i())
    | 41 -> GetMember (s())
    | 42 -> SetMember (s())
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
    | t -> failwithf "bad opcode tag %d" t

let private writeArray (w: BinaryWriter) (writeItem: 'a -> unit) (xs: 'a array) =
    w.Write xs.Length
    for x in xs do writeItem x

let private readArray (r: BinaryReader) (readItem: unit -> 'a) : 'a array =
    let n = r.ReadInt32()
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
    Seq.init n (fun _ -> let k = r.ReadString() in k, r.ReadInt32()) |> Map.ofSeq

let serialize (prog: BytecodeProgram) : byte[] =
    use ms = new MemoryStream()
    use w = new BinaryWriter(ms)
    w.Write magic
    w.Write version
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
        w.Write f.StartPC) prog.Functions
    writeArray w (fun (c: ClassDef) ->
        w.Write c.Name
        writeArray w (fun (s: string) -> w.Write s) c.Fields
        writeMap w c.Methods
        writeMap w c.PropertyGetters
        writeMap w c.PropertyLetters
        writeMap w c.PropertySetters) prog.Classes
    w.Flush()
    ms.ToArray()

let deserialize (bytes: byte[]) : BytecodeProgram =
    use ms = new MemoryStream(bytes)
    use r = new BinaryReader(ms)
    let m = r.ReadInt32()
    let v = r.ReadInt32()
    if m <> magic then failwithf "not a bytecode blob (magic %08x)" m
    if v <> version then failwithf "unsupported bytecode version %d" v
    let code = readArray r (fun () -> readInstr r)
    let constants = readArray r (fun () -> readValue r)
    let globals = r.ReadInt32()
    let globalNames = readArray r (fun () -> r.ReadString())
    let functions =
        readArray r (fun () ->
            let name = r.ReadString()
            let p = r.ReadInt32()
            let l = r.ReadInt32()
            let byref = readArray r (fun () -> r.ReadBoolean())
            let fcode = readArray r (fun () -> readInstr r)
            let start = r.ReadInt32()
            { Name = name; ParamsCount = p; LocalsCount = l; ByRefParams = byref; Code = fcode; StartPC = start })
    let classes =
        readArray r (fun () ->
            let name = r.ReadString()
            let fields = readArray r (fun () -> r.ReadString())
            let methods = readMap r
            let getters = readMap r
            let letters = readMap r
            let setters = readMap r
            { Name = name; Fields = fields; Methods = methods
              PropertyGetters = getters; PropertyLetters = letters; PropertySetters = setters })
    { Code = code; Constants = constants; Globals = globals; GlobalNames = globalNames
      Functions = functions; Classes = classes }
#endif
