module Rbt.Vb.ValueSemantics

open System
open Values

let rec dereference = function
    | VRef (values, index) -> dereference values.[index]
    | VFieldRef (instance, name) -> dereference instance.Fields.[name]
    | value -> value

let private typeMismatch () = raiseVbError 13
let private nullError () = raiseVbError 94

/// The default property of a host object (Let-coercion): `GetMember ""`, then
/// `"_Default"`, then `"Value"`. An object without one raises error 438.
let hostDefault (host: IHostObject) =
    match host.GetMember "" with
    | Some value -> value
    | None ->
        match host.GetMember "_Default" with
        | Some value -> value
        | None ->
            match host.GetMember "Value" with
            | Some value -> value
            | None -> raiseVbError 438

/// Dereference and, for host objects, read through the default property
/// (repeatedly, for a default that is itself an object).
let rec scalar value =
    match dereference value with
    | VHostObject host ->
        match hostDefault host with
        | VHostObject next when obj.ReferenceEquals(next, host) -> raiseVbError 438
        | next -> scalar next
    | other -> other

let private parseNumber (text: string) =
    match Double.TryParse(text.Trim()) with
    | true, number when not (Double.IsNaN number || Double.IsInfinity number) -> number
    | _ -> typeMismatch ()

let toNumber value =
    match value with
    | VInteger number -> float number
    | VDouble number -> number
    | _ ->
    match scalar value with
    | VInteger number -> float number
    | VDouble number -> number
    | VBoolean value -> if value then -1.0 else 0.0
    | VEmpty -> 0.0
    | VString text -> parseNumber text
    | VNull -> nullError ()
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> typeMismatch ()

let private checkedInt (minimum: float) (maximum: float) (number: float) =
    if Double.IsNaN number || Double.IsInfinity number || number < minimum || number > maximum then
        raiseVbError 6
    int number

let toInteger value =
    match value with
    | VInteger number -> number
    | _ ->
        let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
        checkedInt (float Int32.MinValue) (float Int32.MaxValue) rounded

let rec toString value =
    match value with
    | VString text -> text
    | _ ->
    match scalar value with
    | VInteger number -> string number
    | VDouble number -> string number
    | VString text -> text
    | VBoolean value -> if value then "True" else "False"
    | VEmpty -> ""
    | VNull -> nullError ()
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> typeMismatch ()

let toBool value =
    match scalar value with
    | VBoolean boolean -> boolean
    | VEmpty -> false
    | VNull -> nullError ()
    | VString text when String.Equals(text.Trim(), "True", StringComparison.OrdinalIgnoreCase) -> true
    | VString text when String.Equals(text.Trim(), "False", StringComparison.OrdinalIgnoreCase) -> false
    | value -> toNumber value <> 0.0

let inline private fitsInt (number: float) = number >= -2147483648.0 && number <= 2147483647.0

/// Operand for arithmetic: Empty acts as the Integer 0.
let private arithmetic value =
    match scalar value with
    | VEmpty -> VInteger 0
    | other -> other

/// Integer results that leave the 32-bit range are promoted to Double, as VB
/// does for Variants; typed Integer/Long variables raise Overflow on store.
let addValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l + float r
        if fitsInt f then VInteger (l + r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l + r)
    | VDouble l, VInteger r -> VDouble (l + float r)
    | VInteger l, VDouble r -> VDouble (float l + r)
    | _ ->
    match scalar left, scalar right with
    | VString l, VString r -> VString (l + r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l + float r
        if fitsInt f then VInteger (l + r) else VDouble f
    | l, r -> VDouble (toNumber l + toNumber r)

let subtractValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l - float r
        if fitsInt f then VInteger (l - r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l - r)
    | VDouble l, VInteger r -> VDouble (l - float r)
    | VInteger l, VDouble r -> VDouble (float l - r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l - float r
        if fitsInt f then VInteger (l - r) else VDouble f
    | l, r -> VDouble (toNumber l - toNumber r)

let multiplyValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l * float r
        if fitsInt f then VInteger (l * r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l * r)
    | VDouble l, VInteger r -> VDouble (l * float r)
    | VInteger l, VDouble r -> VDouble (float l * r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l * float r
        if fitsInt f then VInteger (l * r) else VDouble f
    | l, r -> VDouble (toNumber l * toNumber r)

let negateValue value =
    match scalar value with
    | VInteger i when i <> Int32.MinValue -> VInteger (-i)
    | VInteger i -> VDouble (-(float i))
    | VDouble d -> VDouble (-d)
    | VNull -> VNull
    | other -> VDouble (-(toNumber other))

// VB arithmetic treats Empty as zero, True as -1, and propagates Null.
// Preserve an integral result only when both operands really are integers;
// coercing a Double or numeric string must not truncate it to an integer.
let numericBinOp (integerOp: int -> int -> int) (doubleOp: float -> float -> float) left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r -> VInteger (integerOp l r)
    | l, r -> VDouble (doubleOp (toNumber l) (toNumber r))

// Binary comparison is the VBScript / VBA default (Option Compare Binary).
// Explicit text comparison is case-insensitive; use one policy for builtins
// and the VM rather than implementing a different ordering in each.
let compareStrings textMode (left: string) (right: string) =
    if textMode then String.Compare(left, right, StringComparison.OrdinalIgnoreCase)
    else String.Compare(left, right, StringComparison.Ordinal)

let compareValues textMode left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VString l, VString r -> VInteger (sign (compareStrings textMode l r))
    | VString l, VEmpty -> VInteger (sign (compareStrings textMode l ""))
    | VEmpty, VString r -> VInteger (sign (compareStrings textMode "" r))
    | l, r -> VInteger (compare (toNumber l) (toNumber r))

let numericCmp (integerOp: int -> int -> bool) (doubleOp: float -> float -> bool) left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r -> VBoolean (integerOp l r)
    | l, r -> VBoolean (doubleOp (toNumber l) (toNumber r))

let roundedInteger minimum maximum value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    checkedInt minimum maximum rounded

let floorValue value =
    match scalar value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | other -> VDouble (Math.Floor(toNumber other))

let truncateValue value =
    match scalar value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | other -> VDouble (Math.Truncate(toNumber other))

// ── Declared types ──

/// Type codes for `Dim x As <type>` coercion (see Bytecode.Coerce).
[<RequireQualifiedAccess>]
type DeclaredType =
    | Integer = 1
    | Long = 2
    | Single = 3
    | Double = 4
    | Currency = 5
    | String = 6
    | Boolean = 7
    | Byte = 8
    | Object = 9
    | Date = 10

let declaredTypeOf (name: string) =
    match name.ToLowerInvariant() with
    | "integer" -> Some DeclaredType.Integer
    | "long" | "longlong" | "longptr" -> Some DeclaredType.Long
    | "single" -> Some DeclaredType.Single
    | "double" -> Some DeclaredType.Double
    | "currency" | "decimal" -> Some DeclaredType.Currency
    | "string" -> Some DeclaredType.String
    | "boolean" -> Some DeclaredType.Boolean
    | "byte" -> Some DeclaredType.Byte
    | "object" -> Some DeclaredType.Object
    | "date" -> Some DeclaredType.Date
    | _ -> None

let defaultOf (t: DeclaredType) =
    match t with
    | DeclaredType.Integer | DeclaredType.Long | DeclaredType.Byte -> VInteger 0
    | DeclaredType.Single | DeclaredType.Double | DeclaredType.Currency | DeclaredType.Date -> VDouble 0.0
    | DeclaredType.String -> VString ""
    | DeclaredType.Boolean -> VBoolean false
    | DeclaredType.Object -> VNothing
    | _ -> VEmpty

/// Convert a value assigned to a variable of a declared type.
let coerceTo (t: DeclaredType) (value: Value) =
    match t, value with
    | DeclaredType.Long, VInteger _ -> value
    | DeclaredType.Double, VDouble _ -> value
    | DeclaredType.String, VString _ -> value
    | DeclaredType.Boolean, VBoolean _ -> value
    | DeclaredType.Object, (VObject _ | VHostObject _ | VNothing) -> value
    | DeclaredType.Object, _ -> raiseVbError 13
    | _ ->
    match t, scalar value with
    | _, VNull -> raiseVbError 94
    | DeclaredType.Integer, v -> VInteger (roundedInteger -32768.0 32767.0 v)
    | DeclaredType.Long, v -> VInteger (roundedInteger -2147483648.0 2147483647.0 v)
    | DeclaredType.Byte, v -> VInteger (roundedInteger 0.0 255.0 v)
    | DeclaredType.Single, v -> VDouble (float (float32 (toNumber v)))
    | (DeclaredType.Double | DeclaredType.Currency), v -> VDouble (toNumber v)
    | DeclaredType.Date, (VString _ as v) -> v
    | DeclaredType.Date, v -> VDouble (toNumber v)
    | DeclaredType.String, v -> VString (toString v)
    | DeclaredType.Boolean, v -> VBoolean (toBool v)
    | _, v -> v
