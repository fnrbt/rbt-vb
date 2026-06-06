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
    | VHostObject of IHostObject
    | VRef of Value array * int
    | VUndefined

and VBObject = {
    ClassName: string
    Fields: Dictionary<string, Value>
    ClassIndex: int
}

and IHostObject =
    abstract TypeName: string
    abstract GetMember: string -> Value option
    abstract SetMember: string -> Value -> bool
    abstract CallMethod: string -> Value array -> Value option
