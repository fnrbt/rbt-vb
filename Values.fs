module Rbt.Vb.Values

open System.Collections.Generic

type Value =
    | VInteger of int
    | VDouble of float
    | VString of string
    | VBoolean of bool
    | VNull
    | VEmpty
    | VNothing
    | VArray of VBArray
    | VObject of VBObject
    | VHostObject of IHostObject
    | VRef of Value array * int
    | VFieldRef of VBObject * string
    /// The value of an omitted Optional Variant argument (`IsMissing` is True).
    | VUndefined

and VBArray = {
    Data: Value array
    LowerBounds: int array
    Lengths: int array
}

and VBObject = {
    ClassName: string
    Fields: Dictionary<string, Value>
    ClassIndex: int
}

/// An object supplied by the embedding host.
///
/// Every member returns `None` (or `false`) when the object has no member of
/// that name; the engine then raises error 438 ("Object doesn't support this
/// property or method"). A member that exists but has no value returns
/// `Some VEmpty`. The empty name `""` addresses the default member: reading a
/// host object in a value context calls `GetMember ""` (falling back to
/// `"_Default"` and then `"Value"`), `obj(args)` calls `CallMethod "" args`
/// (falling back to `"_Default"` and `"Item"`), and `obj = v` / `obj(args) = v`
/// call `SetMember "" v` (on the object returned by `CallMethod "" args`).
///
/// To raise a VB error with a specific number from any member, raise
/// `VbError` (see `Values.raiseVbError`); `On Error` handlers see that number.
and IHostObject =
    abstract TypeName: string
    abstract GetMember: name: string -> Value option
    abstract SetMember: name: string -> value: Value -> bool
    abstract CallMethod: name: string -> args: Value array -> Value option
    /// A call with named arguments (`ws.Range("A1").Find What:="x"`):
    /// `names.[i]` is the name of `args.[i]`, or "" for a positional argument.
    /// Only used when at least one argument is named.
    abstract CallNamed: name: string -> args: Value array -> names: string array -> Value option
    /// `For Each x In obj`; `None` when the object is not a collection.
    abstract Enumerate: unit -> Value seq option

/// A convenience base for host objects: override what the object supports.
/// Unknown members report `None`/`false` (error 438); `CallNamed` ignores the
/// names and calls `CallMethod` positionally; `Enumerate` reports `None`.
[<AbstractClass>]
type HostObject(typeName: string) =
    abstract GetMember: name: string -> Value option
    default _.GetMember _ = None
    abstract SetMember: name: string -> value: Value -> bool
    default _.SetMember _ _ = false
    abstract CallMethod: name: string -> args: Value array -> Value option
    default _.CallMethod _ _ = None
    abstract CallNamed: name: string -> args: Value array -> names: string array -> Value option
    default this.CallNamed name args _ = this.CallMethod name args
    abstract Enumerate: unit -> Value seq option
    default _.Enumerate () = None
    interface IHostObject with
        member _.TypeName = typeName
        member this.GetMember name = this.GetMember name
        member this.SetMember name value = this.SetMember name value
        member this.CallMethod name args = this.CallMethod name args
        member this.CallNamed name args names = this.CallNamed name args names
        member this.Enumerate () = this.Enumerate ()

/// A VB run-time error: Err.Number, Err.Description and Err.Source.
type VbErrorInfo = {
    Number: int
    Description: string
    Source: string
}

exception VbError of VbErrorInfo

/// Standard descriptions of the run-time errors the engine raises.
let errorDescription number =
    match number with
    | 3 -> "Return without GoSub"
    | 5 -> "Invalid procedure call or argument"
    | 6 -> "Overflow"
    | 7 -> "Out of memory"
    | 9 -> "Subscript out of range"
    | 10 -> "This array is fixed or temporarily locked"
    | 11 -> "Division by zero"
    | 13 -> "Type mismatch"
    | 17 -> "Can't perform requested operation"
    | 18 -> "User interrupt occurred"
    | 20 -> "Resume without error"
    | 28 -> "Out of stack space"
    | 35 -> "Sub or Function not defined"
    | 51 -> "Internal error"
    | 91 -> "Object variable or With block variable not set"
    | 92 -> "For loop not initialized"
    | 94 -> "Invalid use of Null"
    | 424 -> "Object required"
    | 429 -> "ActiveX component can't create object"
    | 438 -> "Object doesn't support this property or method"
    | 440 -> "Automation error"
    | 448 -> "Named argument not found"
    | 449 -> "Argument not optional"
    | 450 -> "Wrong number of arguments or invalid property assignment"
    | 451 -> "Object not a collection"
    | 1004 -> "Application-defined or object-defined error"
    | _ -> "Application-defined or object-defined error"

/// Raise a VB run-time error with a custom description.
let raiseVbErrorWith number (description: string) : 'a =
    raise (VbError { Number = number; Description = description; Source = "" })

/// Raise a VB run-time error with its standard description.
let raiseVbError number : 'a = raiseVbErrorWith number (errorDescription number)

/// Callbacks the engine uses for user interaction and object creation.
type RuntimeHooks = {
    /// `Debug.Print`, `Print` and `WScript.Echo` output (one call per line).
    Print: string -> unit
    /// `MsgBox prompt, buttons, title` returns the button code (vbOK = 1 ...).
    MsgBox: string -> int -> string -> int
    /// `InputBox prompt, title, default`; `None` means Cancel (returns "").
    InputBox: string -> string -> string -> string option
    /// `CreateObject progId`; `None` raises error 429.
    CreateObject: string -> Value option
}
