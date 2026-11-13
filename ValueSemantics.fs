module Rbt.Vb.ValueSemantics

open System
open Values

let rec dereference = function
    | VRef (values, index) -> dereference values.[index]
    | VFieldRef (instance, name) -> dereference instance.Fields.[name]
    | value -> value

let private typeMismatch () = failwith "Type mismatch"
let private nullError () = failwith "Invalid use of Null"

let private parseNumber (text: string) =
    match Double.TryParse(text.Trim()) with
    | true, number when not (Double.IsNaN number || Double.IsInfinity number) -> number
    | _ -> typeMismatch ()

let toNumber value =
    match dereference value with
    | VInteger number -> float number
    | VDouble number -> number
    | VBoolean value -> if value then -1.0 else 0.0
    | VEmpty -> 0.0
    | VString text -> parseNumber text
    | VNull -> nullError ()
    | _ -> typeMismatch ()

let private checkedInt (minimum: float) (maximum: float) (number: float) =
    if Double.IsNaN number || Double.IsInfinity number || number < minimum || number > maximum then
        failwith "Overflow"
    int number

let toInteger value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    checkedInt (float Int32.MinValue) (float Int32.MaxValue) rounded

let rec toString value =
    match dereference value with
    | VInteger number -> string number
    | VDouble number -> string number
    | VString text -> text
    | VBoolean value -> if value then "True" else "False"
    | VEmpty -> ""
    | VNull -> nullError ()
    | _ -> typeMismatch ()

let toBool value =
    match dereference value with
    | VBoolean boolean -> boolean
    | VEmpty -> false
    | VNull -> nullError ()
    | VString text when String.Equals(text.Trim(), "True", StringComparison.OrdinalIgnoreCase) -> true
    | VString text when String.Equals(text.Trim(), "False", StringComparison.OrdinalIgnoreCase) -> false
    | value -> toNumber value <> 0.0

// VB arithmetic treats Empty as zero, True as -1, and propagates Null.
// Preserve an integral result only when both operands really are integers;
// coercing a Double or numeric string must not truncate it to an integer.
let numericBinOp (integerOp: int -> int -> int) (doubleOp: float -> float -> float) left right =
    match dereference left, dereference right with
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
    match dereference left, dereference right with
    | VNull, _ | _, VNull -> VNull
    | VString l, VString r -> VInteger (sign (compareStrings textMode l r))
    | VString l, VEmpty -> VInteger (sign (compareStrings textMode l ""))
    | VEmpty, VString r -> VInteger (sign (compareStrings textMode "" r))
    | l, r -> VInteger (compare (toNumber l) (toNumber r))

let numericCmp (integerOp: int -> int -> bool) (doubleOp: float -> float -> bool) left right =
    match dereference left, dereference right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r -> VBoolean (integerOp l r)
    | l, r -> VBoolean (doubleOp (toNumber l) (toNumber r))

let roundedInteger minimum maximum value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    checkedInt minimum maximum rounded

let floorValue value =
    match dereference value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | other -> VDouble (Math.Floor(toNumber other))

let truncateValue value =
    match dereference value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | other -> VDouble (Math.Truncate(toNumber other))
