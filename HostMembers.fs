/// An object model for macros as tables: each type of object (Range, Shape, Document...) is a table of
/// its members by name, built once, and an object is the table with the state of one instance. A member
/// is a property (read, write or both), a method (with its parameters' names, for named arguments) or a
/// property that takes arguments. Names are found in any case, by one dictionary lookup; a name the table
/// does not have is error 438 naming the type and the member.
///
///     let shapeType =
///         HostMembers.Table<Shape>("Shape", [
///             HostMembers.prop "Left" (fun s -> VDouble s.Left) (fun s v -> s.Left <- toFloat v)
///             HostMembers.get "Type" (fun _ -> VLong 1)
///             HostMembers.method "IncrementLeft" [ "Increment" ] (fun s args -> ...; VEmpty) ])
///     shapeType.Wrap shape          // a Value: VHostObject
module Rbt.Vb.HostMembers

open System.Collections.Generic
open Rbt.Vb.Values

/// A member of a type of object whose instances have state 'T.
type Member<'T> = {
    Name: string
    /// Read (`x = obj.Name`); for a method, None.
    Get: ('T -> Value) option
    /// Written (`obj.Name = x`, `Set obj.Name = x`).
    Set: ('T -> Value -> unit) option
    /// Called with arguments (`obj.Name(a, b)`, `obj.Name a, b`); a method without arguments is also read
    /// (`obj.Delete`). A property without Call that is given arguments has them applied to what it reads
    /// (`obj.Rows(3)`: the third of Rows), by the engine.
    Call: ('T -> Value array -> Value) option
    /// The parameters' names, in order, for named arguments (`ScaleHeight Factor:=2`).
    Parameters: string list
}

/// A property that is read only.
let get (name: string) (read: 'T -> Value) : Member<'T> =
    { Name = name; Get = Some read; Set = None; Call = None; Parameters = [] }

/// A property that is read and written.
let prop (name: string) (read: 'T -> Value) (write: 'T -> Value -> unit) : Member<'T> =
    { Name = name; Get = Some read; Set = Some write; Call = None; Parameters = [] }

/// A property that is only written.
let set (name: string) (write: 'T -> Value -> unit) : Member<'T> =
    { Name = name; Get = None; Set = Some write; Call = None; Parameters = [] }

/// A method, with its parameters' names.
let method (name: string) (parameters: string list) (run: 'T -> Value array -> Value) : Member<'T> =
    { Name = name; Get = None; Set = None; Call = Some run; Parameters = parameters }

/// A property that takes arguments (`Cells(r, c)`, `Item(i)`); read without them, `read`.
let indexed (name: string) (parameters: string list) (read: 'T -> Value) (call: 'T -> Value array -> Value) : Member<'T> =
    { Name = name; Get = Some read; Set = None; Call = Some call; Parameters = parameters }

/// A setting the host keeps no state for: it reads as `value`, and a write is accepted and has no effect
/// (Excel's DisplayPageBreaks, a Shape's LockAspectRatio).
let accepted (name: string) (value: Value) : Member<'T> =
    { Name = name; Get = Some (fun _ -> value); Set = Some (fun _ _ -> ()); Call = None; Parameters = [] }

/// The same member under another name (`Caption` for `Text`).
let alias (name: string) (target: Member<'T>) : Member<'T> = { target with Name = name }

/// The arguments of a call, by position: `arg args i` is VUndefined (an omitted argument) past their end.
let arg (args: Value array) (index: int) = if index < args.Length then args.[index] else VUndefined

let private notSupported (typeName: string) (name: string) : 'a =
    raiseVbErrorWith 438 (sprintf "Object doesn't support this property or method (%s.%s)" typeName name)

/// What a type does with names that are not in its table: read (args None) or called (Some args), or
/// written. None: not a member (error 438).
type Dynamic<'T> = {
    Read: 'T -> string -> Value array option -> Value option
    Write: 'T -> string -> Value -> bool
}

/// A type of object: its name (TypeName), its members, the default member (`obj(...)`, `obj = x`, the
/// object read as a value) and, for a collection, its items (For Each). Its members are numbered (their
/// place in the list), so the engine can reach them by number (HostDispatch).
[<AllowNullLiteral>]
type Table<'T>(typeName: string, members: Member<'T> list, ?defaultMember: string, ?items: 'T -> Value seq,
               ?dynamic: Dynamic<'T>, ?typeNameOf: 'T -> string) =
    let numbered = List.toArray members
    let index = Dictionary<string, int>()
    do
        numbered |> Array.iteri (fun i m -> index.[m.Name.ToLowerInvariant()] <- i)
    let defaultKey = defaultMember |> Option.map (fun name -> name.ToLowerInvariant())

    member _.TypeName = typeName
    /// The members, as the table names them (for an editor's completion, documentation, parity checks).
    member _.Names = members |> List.map (fun m -> m.Name)
    /// The number of a member by name (any case; "" and _Default: the default member), or -1.
    member _.Resolve(name: string) : int =
        let key =
            match name with
            | "" | "_Default" | "_default" -> (match defaultKey with Some key -> key | None -> "")
            | _ -> name.ToLowerInvariant()
        match index.TryGetValue key with
        | true, i -> i
        | _ -> -1
    member _.Member(id: int) : Member<'T> = numbered.[id]
    member this.TryFind(name: string) : Member<'T> option =
        match this.Resolve name with
        | -1 -> None
        | i -> Some numbered.[i]
    member _.Items = items
    member _.Dynamic = dynamic
    member _.TypeNameOf(state: 'T) = match typeNameOf with Some f -> f state | None -> typeName
    member _.HasDefault = defaultKey.IsSome

    /// An object of this type with that state.
    member this.Wrap(state: 'T) : Value = VHostObject (TableObject<'T>(this, state))

    /// The state of an object of this type (None for any other value).
    member this.TryState(value: Value) : 'T option =
        match value with
        | VHostObject host ->
            match box host with
            | :? TableObject<'T> as o when obj.ReferenceEquals(o.Table, this) -> Some o.State
            | _ -> None
        | _ -> None

/// An instance: its type's table and its state.
and TableObject<'T>(table: Table<'T>, state: 'T) =
    inherit HostDispatch()
    member _.Table = table
    member _.State = state

    // (By number: what the member answers by itself; None / false when the name has to decide - a property
    // with arguments, a member that cannot be read or written.)
    override _.DispatchType = box table
    override _.Resolve name = table.Resolve name
    override _.GetById id =
        match table.Member id with
        | { Get = Some read } -> Some (read state)
        | { Call = Some run } -> Some (run state [||])
        | _ -> None
    override _.SetById id value =
        match table.Member id with
        | { Set = Some write } -> write state value; true
        | _ -> false
    override _.CallById id args =
        match table.Member id with
        | { Call = Some run } -> Some (run state args)
        | { Get = Some read } when args.Length = 0 -> Some (read state)
        | _ -> None

    interface IHostObject with
        member _.TypeName = table.TypeNameOf state
        member this.GetMember name =
            match table.Resolve name with
            | -1 -> this.GetOther name
            | id -> match this.GetById id with Some value -> Some value | None -> this.GetOther name
        member this.SetMember name value =
            match table.Resolve name with
            | -1 -> this.SetOther name value
            | id -> if this.SetById id value then true else this.SetOther name value
        member this.CallMethod name args =
            match table.Resolve name with
            | -1 -> this.CallOther name args
            | id ->
                match table.Member id with
                | { Call = Some run } -> Some (run state args)
                | { Get = Some read } when args.Length = 0 -> Some (read state)
                // (A property given arguments: the engine applies them to what it reads.)
                | { Get = Some _ } -> None
                | _ -> this.CallOther name args
        member this.CallNamed name args names =
            let self = this :> IHostObject
            if names |> Array.forall (fun n -> n = "") then self.CallMethod name args
            else
                match table.TryFind name with
                | Some m when not m.Parameters.IsEmpty ->
                    // Named arguments into their places; the holes are omitted arguments.
                    let parameters = m.Parameters |> List.map (fun p -> p.ToLowerInvariant())
                    let slots = ResizeArray<Value>()
                    let put index value =
                        while slots.Count <= index do slots.Add VUndefined
                        slots.[index] <- value
                    args |> Array.iteri (fun i value ->
                        let argName = if i < names.Length then names.[i] else ""
                        if argName = "" then put i value
                        else
                            match parameters |> List.tryFindIndex ((=) (argName.ToLowerInvariant())) with
                            | Some position -> put position value
                            | None -> raiseVbErrorWith 448 ("Named argument not found: " + argName))
                    self.CallMethod name (slots.ToArray())
                | _ -> self.CallMethod name args
        member _.Enumerate () = table.Items |> Option.map (fun items -> items state)

    // A name the table does not answer: the type's dynamic members, else error 438.
    member private _.GetOther(name: string) =
        match table.Dynamic |> Option.bind (fun d -> d.Read state name None) with
        | Some value -> Some value
        | None ->
            // (The engine reads an object as a value through "" and _Default: None lets it go on.)
            if name = "" || name = "_Default" || name = "Value" && not table.HasDefault then None
            else notSupported (table.TypeNameOf state) name
    member private _.SetOther (name: string) (value: Value) =
        match table.Dynamic with
        | Some d when d.Write state name value -> true
        | _ -> if name = "" then false else notSupported (table.TypeNameOf state) name
    member private _.CallOther (name: string) (args: Value array) =
        match table.Dynamic |> Option.bind (fun d -> d.Read state name (Some args)) with
        | Some value -> Some value
        | None ->
            if name = "" || name = "_Default" || name = "Item" then None
            else notSupported (table.TypeNameOf state) name
