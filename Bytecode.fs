module Bytecode

open Values

type Opcode =
    | Nop
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
    | ArrayNew
    | ArrayGet
    | ArraySet
    | ArrayLength
    | ReDimPreserve
    // Objects
    | NewObj of int           // class index
    | GetMember of string     // member name
    | SetMember of string     // member name
    | CallMethod of string * int  // method name, arg count
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
}

type ClassDef = {
    Name: string
    Fields: string array
    Methods: Map<string, int>          // method name -> FunctionDef index
    PropertyGetters: Map<string, int>
    PropertyLetters: Map<string, int>
    PropertySetters: Map<string, int>
}

type BytecodeProgram = {
    Code: Instruction array
    Constants: Value array
    Globals: int
    Functions: FunctionDef array
    Classes: ClassDef array
}
