module Ast

type Dialect = VBA | VBScript

type TokenKind =
    | Identifier
    | Number
    | StringLiteral
    | Comment
    | Keyword
    | Operator
    | LParen
    | RParen
    | Comma
    | Colon
    | Semicolon
    | Dot
    | Eq
    | Newline
    | EOF

type Token = {
    Kind: TokenKind
    Lexeme: string
    Line: int
    Column: int
    PrecedingNewline: bool
}

type BinaryOperator =
    | Add | Subtract | Multiply | Divide | IntDivide | Modulus
    | Power
    | Concatenate
    | Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual
    | And | Or | Xor | Eqv | Imp
    | Is | Like

type UnaryOperator = Not | Negate

type Literal =
    | Integer of int
    | Double of float
    | String of string
    | Boolean of bool
    | Null
    | Empty
    | Nothing

type Visibility = Public | Private | Friend | Global

type TypeRef = {
    Name: string
}

type ArraySpec = {
    Dimensions: Expression list  // dimension bounds
}

and Expression =
    | Literal of Literal
    | Identifier of string
    | Binary of BinaryOperator * Expression * Expression
    | Unary of UnaryOperator * Expression
    | Call of Expression * Expression list
    | Member of Expression * string
    | NewExpr of string  // New ClassName
    | MeExpr
    | TypeOfIs of Expression * string
    | WithDotExpr

type VarDeclarator = {
    Name: string
    ArraySpec: ArraySpec option
    Type: TypeRef option         // null in VBS
}

type Declaration =
    | Dim of VarDeclarator list
    | Const of string * TypeRef option * Expression
    | ReDim of string * Expression * bool  // name, size, preserve flag

type Parameter = {
    Name: string
    ByRef: bool
    IsArray: bool                // trailing () on param
    Optional: bool              // VBA only
    IsParamArray: bool          // VBA only
    Type: TypeRef option        // VBA only
    DefaultValue: Expression option  // VBA only
}

type OnErrorAction =
    | ResumeNext
    | GoToZero
    | GoToLabel of string       // VBA only

type CaseTest =
    | CaseValue of Expression
    | CaseRange of Expression * Expression
    | CaseComparison of BinaryOperator * Expression

type Statement =
    | ExpressionStmt of Expression
    | Let of string * Expression
    | Set of string * Expression
    | IfStmt of Expression * Statement list * (Expression * Statement list) list * Statement list option
    | SelectCase of Expression * (CaseTest list option * Statement list) list
    | ForLoop of string * Expression * Expression * Expression option * Statement list
    | ForEach of string * Expression * Statement list
    | WhileLoop of Expression * Statement list
    | DoLoop of DoCondition option * DoCondition option
    | ExitFor | ExitDo | ExitSub | ExitFunction | ExitProperty
    | CallStmt of Expression * Expression list
    | GoToStmt of string
    | GoSubStmt of string
    | ReturnStmt
    | LabelStmt of string
    | Declaration of Visibility option * Declaration
    | Assignment of string * Expression
    | IndexedAssignment of string * Expression list * Expression
    | MemberAssignment of Expression * string * Expression
    | OnError of OnErrorAction
    | WithStmt of Expression * Statement list
    | EraseStmt of string

and DoCondition = {
    Condition: Expression
    Body: Statement list
}

type Modifier =
    | VisibilityMod of Visibility
    | StaticMod
    | DefaultMod

type ProcKind = SubProc | FunctionProc | PropertyProc

type Function =
    | FunctionDecl of Modifier list * string * Parameter list * TypeRef option * Statement list
    | SubDecl of Modifier list * string * Parameter list * Statement list
    | PropertyGet of Modifier list * string * Parameter list * TypeRef option * Statement list
    | PropertyLet of Modifier list * string * Parameter list * Statement list
    | PropertySet of Modifier list * string * Parameter list * Statement list

type EnumMember = {
    Name: string
    Value: Expression option
}

type TypeMember = {
    Name: string
    Type: TypeRef
    ArraySpec: ArraySpec option
}

type DeclareInfo = {
    IsFunction: bool
    Name: string
    LibName: string
    AliasName: string option
    Parameters: Parameter list
    ReturnType: TypeRef option
}

type TopLevel =
    | FunctionDef of Function
    | ClassDecl of Visibility option * string * TopLevel list
    | EnumDecl of Visibility option * string * EnumMember list        // VBA only
    | TypeDecl of Visibility option * string * TypeMember list        // VBA only
    | DeclareDecl of Visibility option * DeclareInfo                  // VBA only
    | ImplementsDecl of string                                        // VBA only
    | EventDecl of Visibility option * string * Parameter list        // VBA only
    | WithEventsDecl of Visibility option * string * TypeRef          // VBA only
    | OptionStmt of string                                            // Option Explicit, etc.
    | TopLevelStatement of Statement

type Program = {
    Dialect: Dialect
    TopLevels: TopLevel list
}
