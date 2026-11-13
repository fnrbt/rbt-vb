module Rbt.Vb.Builtins

open Values
open ValueSemantics
open System
open System.Text

let private compareMode defaultTextCompare value =
    match toInteger value with
    | 0 -> false
    | 1 -> true
    | -1 -> defaultTextCompare
    | _ -> failwith "Invalid comparison mode"

let private builtinArray (data: Value array) =
    VArray { Data = data; LowerBounds = [| 0 |]; Lengths = [| data.Length |] }

let private arrayBound upper (args: Value array) =
    let array =
        match dereference args.[0] with
        | VArray array -> array
        | _ -> failwith "Type mismatch"
    let dimension = if args.Length >= 2 then toInteger args.[1] else 1
    if dimension < 1 || dimension > array.Lengths.Length then failwith "Subscript out of range"
    let index = dimension - 1
    let lower = array.LowerBounds.[index]
    VInteger (if upper then lower + array.Lengths.[index] - 1 else lower)
let private rng = Random()

let rec private typeName = function
    | VInteger _ -> "Integer"
    | VDouble _ -> "Double"
    | VString _ -> "String"
    | VBoolean _ -> "Boolean"
    | VNull -> "Null"
    | VEmpty -> "Empty"
    | VNothing -> "Nothing"
    | VArray _ -> "Variant()"
    | VObject o -> o.ClassName
    | VHostObject o -> o.TypeName
    | VRef (arr, idx) when idx >= 0 && idx < arr.Length -> typeName arr.[idx]
    | VRef _ -> "Empty"
    | VFieldRef (instance, name) -> typeName instance.Fields.[name]
    | VUndefined -> "Undefined"

let callBuiltin (defaultTextCompare: bool) (name: string) (args: Value array) : Value =
    let argc = args.Length
    let arg i = if i < argc then dereference args.[i] else VEmpty
    let s i = toString (arg i)
    let n i = toInteger (arg i)
    let d i = toNumber (arg i)

    match name with
    // Output
    | "print" ->
        let sb = StringBuilder()
        for i in 0 .. argc - 1 do
            if i > 0 then sb.Append " " |> ignore
            sb.Append(toString args.[i]) |> ignore
        printfn "%s" (sb.ToString())
        VEmpty

    // String functions
    | "len" -> VInteger (s(0).Length)
    | "left" -> VString (s(0).[.. max 0 (n 1) - 1])
    | "right" ->
        let str = s 0
        let count = min (n 1) str.Length
        VString (str.[str.Length - count ..])
    | "mid" ->
        let str = s 0
        let start = max 1 (n 1) - 1  // 1-based to 0-based
        if start >= str.Length then VString ""
        elif argc >= 3 then
            let len = min (n 2) (str.Length - start)
            VString (str.Substring(start, max 0 len))
        else
            VString (str.[start..])
    | "ucase" -> VString ((s 0).ToUpperInvariant())
    | "lcase" -> VString ((s 0).ToLowerInvariant())
    | "trim" -> VString ((s 0).Trim())
    | "ltrim" -> VString ((s 0).TrimStart())
    | "rtrim" -> VString ((s 0).TrimEnd())
    | "instr" ->
        if argc <> 2 && argc <> 3 && argc <> 4 then failwith "Invalid argument count"
        let first, second = if argc = 2 then arg 0, arg 1 else arg 1, arg 2
        match first, second with
        | VNull, _ | _, VNull -> VNull
        | _ ->
            let start, text, needle, textMode =
                if argc = 2 then 1, s 0, s 1, defaultTextCompare
                else n 0, s 1, s 2, (if argc = 4 then compareMode defaultTextCompare (arg 3) else defaultTextCompare)
            if start < 1 then failwith "Invalid procedure call or argument"
            if text.Length = 0 || start > text.Length then VInteger 0
            elif needle.Length = 0 then VInteger start
            else
#if FABLE_COMPILER
                let index =
                    if textMode then text.ToUpperInvariant().IndexOf(needle.ToUpperInvariant(), start - 1)
                    else text.IndexOf(needle, start - 1)
#else
                let mode = if textMode then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
                let index = text.IndexOf(needle, start - 1, mode)
#endif
                VInteger (if index < 0 then 0 else index + 1)
    | "instrrev" ->
        let str = s 0
        let find = s 1
        let start = if argc >= 3 then min (n 2) str.Length - 1 else str.Length - 1
        if start < 0 || str.Length = 0 then VInteger 0
        else
#if FABLE_COMPILER
            let idx = str.LastIndexOf(find, start)
#else
            let idx = str.LastIndexOf(find, start, StringComparison.Ordinal)
#endif
            VInteger (if idx >= 0 then idx + 1 else 0)
    | "replace" -> VString ((s 0).Replace(s 1, s 2))
    | "space" -> VString (String(' ', max 0 (n 0)))
    | "string" ->
        let count = max 0 (n 0)
        let ch = let str = s 1 in if str.Length > 0 then str.[0] else ' '
        VString (String(ch, count))
    | "strreverse" -> VString (String(Array.rev ((s 0).ToCharArray())))
    | "asc" ->
        let str = s 0
        if str.Length > 0 then VInteger (int str.[0]) else VInteger 0
    | "chr" -> VString (string (char (n 0)))
    | "split" ->
        let text = s 0
        let delim = if argc >= 2 then s 1 else " "
        let parts =
            if text.Length = 0 then [||]
            elif delim.Length = 0 then [| text |]
            else text.Split([|delim|], StringSplitOptions.None)
        builtinArray (parts |> Array.map VString)
    | "join" ->
        match arg 0 with
        | VArray arr when arr.Lengths.Length = 1 ->
            let delim = if argc >= 2 then s 1 else " "
            VString (arr.Data |> Array.map toString |> String.concat delim)
        | _ -> failwith "Type mismatch"
    | "strcomp" ->
        if argc < 2 || argc > 3 then failwith "Invalid argument count"
        match arg 0, arg 1 with
        | VNull, _ | _, VNull -> VNull
        | _ ->
            let textMode = if argc = 3 then compareMode defaultTextCompare (arg 2) else defaultTextCompare
            VInteger (sign (compareStrings textMode (s 0) (s 1)))

    // Type conversion
    | "cint" -> VInteger (roundedInteger -32768.0 32767.0 (arg 0))
    | "clng" -> VInteger (toInteger (arg 0))
    | "int" -> floorValue (arg 0)
    | "fix" -> truncateValue (arg 0)
    | "cdbl" -> VDouble (d 0)
    | "csng" -> VDouble (float (float32 (d 0)))
    | "cstr" -> VString (s 0)
    | "cbool" -> VBoolean (toBool (arg 0))
    | "cbyte" -> VInteger (roundedInteger 0.0 255.0 (arg 0))

    // Type checking
    | "isnull" -> VBoolean (match arg 0 with VNull -> true | _ -> false)
    | "isempty" -> VBoolean (match arg 0 with VEmpty -> true | _ -> false)
    | "isnumeric" ->
        match arg 0 with
        | VInteger _ | VDouble _ -> VBoolean true
        | VString s -> VBoolean (match Double.TryParse(s) with true, _ -> true | _ -> false)
        | _ -> VBoolean false
    | "isarray" -> VBoolean (match arg 0 with VArray _ -> true | _ -> false)
    | "isobject" -> VBoolean (match arg 0 with VObject _ | VHostObject _ | VNothing -> true | _ -> false)
    | "isdate" -> VBoolean false
    | "typename" ->
        VString (typeName (arg 0))
    | "vartype" ->
        let vt =
            match arg 0 with
            | VEmpty -> 0
            | VNull -> 1
            | VInteger _ -> 2
            | VDouble _ -> 5
            | VString _ -> 8
            | VBoolean _ -> 11
            | VArray _ -> 0x2000
            | VObject _ -> 9
            | VHostObject _ -> 9
            | _ -> 0
        VInteger vt

    // Math
    | "abs" ->
        match arg 0 with
        | VInteger i -> VInteger (abs i)
        | VDouble d -> VDouble (abs d)
        | _ -> VDouble (abs (d 0))
    | "sgn" ->
        match arg 0 with
        | VInteger i -> VInteger (sign i)
        | _ -> VInteger (sign (d 0))
    | "sqr" -> VDouble (sqrt (d 0))
    | "exp" -> VDouble (Math.Exp(d 0))
    | "log" -> VDouble (Math.Log(d 0))
    | "sin" -> VDouble (Math.Sin(d 0))
    | "cos" -> VDouble (Math.Cos(d 0))
    | "tan" -> VDouble (Math.Tan(d 0))
    | "atn" -> VDouble (Math.Atan(d 0))
    | "rnd" -> VDouble (rng.NextDouble())
    | "round" ->
        let value = d 0
        let decimals = if argc >= 2 then n 1 else 0
        if decimals < 0 || decimals > 15 then failwith "Invalid procedure call or argument"
        VDouble (Math.Round(value, decimals, MidpointRounding.ToEven))
    | "hex" -> VString (sprintf "%X" (n 0))
    | "oct" -> VString (Convert.ToString(n 0, 8))

    // Array
    | "array" -> builtinArray args
    | "ubound" -> 
        if argc < 1 || argc > 2 then failwith "Invalid argument count"
        arrayBound true args
    | "lbound" ->
        if argc < 1 || argc > 2 then failwith "Invalid argument count"
        arrayBound false args

    // Misc
    | "msgbox" ->
        printfn "%s" (s 0)
        VInteger 1
    | "inputbox" ->
        printf "%s" (s 0)
#if FABLE_COMPILER
        VString ""
#else
        let input = Console.ReadLine()
        VString (if input = null then "" else input)
#endif
    | "now" -> VString (DateTime.Now.ToString())
    | "timer" -> VDouble (float (DateTime.Now.TimeOfDay.TotalSeconds))
    | "formatnumber" ->
        let value = d 0
        let decimals = if argc >= 2 then n 1 else 2
        VString (value.ToString("F" + string decimals))

    | _ -> failwith ("Unknown builtin: " + name)
