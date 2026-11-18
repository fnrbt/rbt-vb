/// VBA's Collection and Scripting.Dictionary, as built-in host objects.
module Rbt.Vb.Collections

open System.Collections.Generic
open Values
open ValueSemantics

let private isMissing (value: Value) =
    match value with
    | VUndefined -> true
    | _ -> false

/// Arrays are copied when stored, as VB copies them on assignment.
let private stored (value: Value) =
    match dereference value with
    | VArray a -> VArray { a with Data = Array.copy a.Data }
    | other -> other

let private lowerKey (text: string) = text.ToLowerInvariant()

type private CollectionItem = { Key: string; Value: Value }

/// `New Collection`: 1-based items with optional case-insensitive string keys.
type VbCollection() =
    inherit HostObject("Collection")
    let items = ResizeArray<CollectionItem>()
    let byKey = Dictionary<string, CollectionItem>()

    let keyOf (value: Value) =
        match scalar value with
        | VString text -> text
        | VNull -> raiseVbError 94
        | other -> toString other

    /// The 0-based position of an index or key; error 9 (index) or 5 (key).
    let position (value: Value) =
        match scalar value with
        | VString text ->
            match byKey.TryGetValue(lowerKey text) with
            | true, item -> items.IndexOf item
            | _ -> raiseVbError 5
        | VObject _ | VHostObject _ | VNothing -> raiseVbError 13
        | other ->
            let index = toInteger other
            if index < 1 || index > items.Count then raiseVbError 9
            index - 1

    member _.Count = items.Count
    member _.Items = items |> Seq.map (fun i -> i.Value) |> Seq.toArray

    member _.Add(item: Value, key: Value, before: Value, after: Value) =
        let key = if isMissing key || dereference key = VEmpty then null else keyOf key
        if not (isNull key) && byKey.ContainsKey(lowerKey key) then raiseVbError 457
        if not (isMissing before) && not (isMissing after) then raiseVbError 5
        let entry = { Key = key; Value = stored item }
        let at =
            if not (isMissing before) then position before
            elif not (isMissing after) then position after + 1
            else items.Count
        items.Insert(at, entry)
        if not (isNull key) then byKey.[lowerKey key] <- entry

    member _.Item(index: Value) = items.[position index].Value

    member _.Remove(index: Value) =
        let at = position index
        let entry = items.[at]
        items.RemoveAt at
        if not (isNull entry.Key) then byKey.Remove(lowerKey entry.Key) |> ignore

    override this.GetMember name =
        match name.ToLowerInvariant() with
        | "count" -> Some (VLong items.Count)
        | _ -> None

    override this.CallMethod name args =
        let arg i = if i < args.Length then args.[i] else VUndefined
        match name.ToLowerInvariant() with
        | "add" ->
            if args.Length < 1 || isMissing args.[0] then raiseVbError 449
            if args.Length > 4 then raiseVbError 450
            this.Add(args.[0], arg 1, arg 2, arg 3)
            Some VEmpty
        | "" | "item" ->
            if args.Length <> 1 then raiseVbError 450
            Some (this.Item args.[0])
        | "remove" ->
            if args.Length <> 1 then raiseVbError 450
            this.Remove args.[0]
            Some VEmpty
        | "count" when args.Length = 0 -> Some (VLong items.Count)
        | _ -> None

    override this.CallNamed name args names =
        match name.ToLowerInvariant() with
        | "add" ->
            let slots = Array.create 4 VUndefined
            args |> Array.iteri (fun i value ->
                let slot =
                    match names.[i].ToLowerInvariant() with
                    | "" -> i
                    | "item" -> 0
                    | "key" -> 1
                    | "before" -> 2
                    | "after" -> 3
                    | other -> raiseVbErrorWith 448 ("Named argument not found: " + other)
                if slot > 3 then raiseVbError 450
                slots.[slot] <- value)
            this.CallMethod "Add" slots
        | _ -> this.CallMethod name args

    override _.Enumerate () = Some (items |> Seq.map (fun i -> i.Value) |> Seq.toArray :> Value seq)

/// A Dictionary key: numbers compare by value whatever their type, strings
/// by CompareMode, objects by identity.
[<CustomEquality; NoComparison>]
type private DictionaryKey =
    | KNumber of float
    | KString of string
    | KObject of obj
    | KEmpty
    | KNull
    override this.Equals other =
        match other with
        | :? DictionaryKey as o ->
            match this, o with
            | KNumber a, KNumber b -> a = b
            | KString a, KString b -> a = b
            | KObject a, KObject b -> obj.ReferenceEquals(a, b)
            | KEmpty, KEmpty | KNull, KNull -> true
            | _ -> false
        | _ -> false
    override this.GetHashCode () =
        match this with
        | KNumber a -> a.GetHashCode()
        | KString a -> a.GetHashCode()
        | KObject a -> LanguagePrimitives.PhysicalHash a
        | KEmpty -> 1
        | KNull -> 2

type private DictionaryEntry = { mutable Key: Value; mutable Item: Value; mutable Removed: bool }

/// `CreateObject("Scripting.Dictionary")`: keys of any type in insertion order.
type VbDictionary() =
    inherit HostObject("Dictionary")
    let entries = ResizeArray<DictionaryEntry>()
    let index = Dictionary<DictionaryKey, DictionaryEntry>()
    let mutable compareMode = 0
    let mutable removed = 0

    let keyOf (value: Value) =
        match dereference value with
        | VString text -> KString (if compareMode = 0 then text else lowerKey text)
        | VInteger n | VLong n -> KNumber (float n)
        | VByte n -> KNumber (float n)
        | VSingle f -> KNumber (float f)
        | VDouble d | VDate d -> KNumber d
        | VCurrency c -> KNumber (float c / 10000.0)
        | VBoolean b -> KNumber (if b then -1.0 else 0.0)
        | VObject o -> KObject (box o.Fields)
        | VHostObject h -> KObject (box h)
        | VEmpty | VUndefined -> KEmpty
        | VNull -> KNull
        | VNothing -> raiseVbError 91
        | VArray _ -> raiseVbError 13
        | VError e -> KNumber (float e)
        | other -> KString (toString other)

    let live () = entries |> Seq.filter (fun e -> not e.Removed)

    let compact () =
        if removed > 16 && removed * 2 > entries.Count then
            entries.RemoveAll(fun e -> e.Removed) |> ignore
            removed <- 0

    let add (key: Value) (item: Value) =
        let k = keyOf key
        if index.ContainsKey k then raiseVbError 457
        let entry = { Key = dereference key; Item = stored item; Removed = false }
        entries.Add entry
        index.[k] <- entry

    member _.Count = index.Count
    member _.Keys = live () |> Seq.map (fun e -> e.Key) |> Seq.toArray
    member _.Items = live () |> Seq.map (fun e -> e.Item) |> Seq.toArray
    member _.Exists(key: Value) = index.ContainsKey(keyOf key)

    member _.Item
        with get (key: Value) =
            let k = keyOf key
            match index.TryGetValue k with
            | true, entry -> entry.Item
            | _ ->
                // Reading a missing key adds it with an Empty item.
                add key VEmpty
                VEmpty
        and set (key: Value) (value: Value) =
            match index.TryGetValue(keyOf key) with
            | true, entry -> entry.Item <- stored value
            | _ -> add key value

    member _.Add(key: Value, item: Value) = add key item

    member _.Remove(key: Value) =
        let k = keyOf key
        match index.TryGetValue k with
        | true, entry ->
            entry.Removed <- true
            index.Remove k |> ignore
            removed <- removed + 1
            compact ()
        | _ -> raiseVbError 32811

    member _.RemoveAll() =
        entries.Clear()
        index.Clear()
        removed <- 0

    member _.CompareMode
        with get () = compareMode
        and set (mode: int) =
            if index.Count > 0 then raiseVbError 5
            if mode < 0 || mode > 2 then raiseVbError 5
            compareMode <- mode

    member _.RenameKey(oldKey: Value, newKey: Value) =
        let o = keyOf oldKey
        let n = keyOf newKey
        match index.TryGetValue o with
        | true, entry ->
            if index.ContainsKey n && n <> o then raiseVbError 457
            index.Remove o |> ignore
            entry.Key <- dereference newKey
            index.[n] <- entry
        | _ -> raiseVbError 32811

    static member private Array (data: Value array) =
        VArray { Data = data; LowerBounds = [| 0 |]; Lengths = [| data.Length |] }

    override this.GetMember name =
        match name.ToLowerInvariant() with
        | "count" -> Some (VLong index.Count)
        | "comparemode" -> Some (VLong compareMode)
        | "keys" -> Some (VbDictionary.Array this.Keys)
        | "items" -> Some (VbDictionary.Array this.Items)
        | "removeall" -> this.RemoveAll(); Some VEmpty
        | _ -> None

    override this.SetMember name value =
        match name.ToLowerInvariant() with
        | "comparemode" -> this.CompareMode <- toInteger value; true
        | _ -> false

    override this.CallMethod name args =
        let need n = if args.Length <> n then raiseVbError (if args.Length < n then 449 else 450)
        match name.ToLowerInvariant() with
        | "add" -> need 2; this.Add(args.[0], args.[1]); Some VEmpty
        | "" | "item" -> need 1; Some (this.Item args.[0])
        | "exists" -> need 1; Some (VBoolean (this.Exists args.[0]))
        | "remove" -> need 1; this.Remove args.[0]; Some VEmpty
        | "removeall" -> need 0; this.RemoveAll(); Some VEmpty
        | "keys" -> need 0; Some (VbDictionary.Array this.Keys)
        | "items" -> need 0; Some (VbDictionary.Array this.Items)
        | "count" -> need 0; Some (VLong index.Count)
        | "comparemode" -> need 0; Some (VLong compareMode)
        | _ -> None

    override this.CallNamed name args names =
        match name.ToLowerInvariant() with
        | "add" ->
            let slots = Array.create 2 VUndefined
            args |> Array.iteri (fun i value ->
                let slot =
                    match names.[i].ToLowerInvariant() with
                    | "" -> i
                    | "key" -> 0
                    | "item" -> 1
                    | other -> raiseVbErrorWith 448 ("Named argument not found: " + other)
                if slot > 1 then raiseVbError 450
                slots.[slot] <- value)
            this.CallMethod "Add" slots
        | _ -> this.CallMethod name args

    override this.SetIndexed name args value =
        match name.ToLowerInvariant(), args with
        | ("" | "item"), [| key |] -> this.Item(key) <- value; true
        | "key", [| key |] -> this.RenameKey(key, value); true
        | _ -> false

    override this.Enumerate () = Some (this.Keys :> Value seq)

/// The objects the engine creates for `CreateObject` before asking the host.
let tryCreate (progId: string) : Value option =
    match progId.Trim().ToLowerInvariant() with
    | "scripting.dictionary" -> Some (VHostObject (VbDictionary()))
    | _ -> None

/// `New <name>` for the built-in classes (when no VB class has the name).
let tryNew (className: string) : Value option =
    match className.ToLowerInvariant() with
    | "collection" | "vba.collection" -> Some (VHostObject (VbCollection()))
    | "dictionary" | "scripting.dictionary" -> Some (VHostObject (VbDictionary()))
    | _ -> None

let isBuiltinClass (className: string) =
    match className.ToLowerInvariant() with
    | "collection" | "vba.collection" | "dictionary" | "scripting.dictionary" -> true
    | _ -> false
