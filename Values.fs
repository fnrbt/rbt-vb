module Values

open System.Collections.Generic

type Value =
    | VInteger of int
    | VDouble of float
    | VString of string
    | VBoolean of bool
    | VNull
    | VEmpty
    | VNothing
    | VArray of Value array
    | VObject of VBObject
    | VRef of Value array * int
    | VUndefined

and VBObject = {
    ClassName: string
    Fields: Dictionary<string, Value>
    ClassIndex: int
}
