module Builtins

open Values
open System
open System.Text

let valueToInt = function
    | VInteger i -> i
    | VDouble d -> int d
    | VBoolean b -> if b then 1 else 0
    | VString s -> match Int32.TryParse(s) with true, i -> i | _ -> 0
    | _ -> 0

let valueToDouble = function
    | VInteger i -> float i
    | VDouble d -> d
    | VBoolean b -> if b then 1.0 else 0.0
    | VString s -> match Double.TryParse(s) with true, d -> d | _ -> 0.0
    | _ -> 0.0

let valueToString = function
    | VInteger i -> string i
    | VDouble d -> string d
    | VString s -> s
    | VBoolean b -> if b then "True" else "False"
    | VNull -> ""
    | VEmpty -> ""
    | VNothing -> ""
    | _ -> ""

let valueToBool = function
    | VBoolean b -> b
    | VInteger i -> i <> 0
    | VDouble d -> d <> 0.0
    | VString s -> s <> ""
    | _ -> false

let private rng = Random()

let callBuiltin (name: string) (args: Value array) : Value =
    let argc = args.Length
    let arg i = if i < argc then args.[i] else VEmpty
    let s i = valueToString (arg i)
    let n i = valueToInt (arg i)
    let d i = valueToDouble (arg i)

    match name with
    // Output
    | "print" ->
        let sb = StringBuilder()
        for i in 0 .. argc - 1 do
            if i > 0 then sb.Append " " |> ignore
            sb.Append(valueToString args.[i]) |> ignore
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
        if argc >= 3 then
            // InStr(start, string1, string2)
            let start = max 1 (n 0) - 1
            let str = s 1
            let find = s 2
            let idx = str.IndexOf(find, start, StringComparison.Ordinal)
            VInteger (if idx >= 0 then idx + 1 else 0)
        else
            // InStr(string1, string2)
            let idx = (s 0).IndexOf(s 1, StringComparison.Ordinal)
            VInteger (if idx >= 0 then idx + 1 else 0)
    | "instrrev" ->
        let str = s 0
        let find = s 1
        let start = if argc >= 3 then min (n 2) str.Length - 1 else str.Length - 1
        if start < 0 || str.Length = 0 then VInteger 0
        else
            let idx = str.LastIndexOf(find, start, StringComparison.Ordinal)
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
        let str = s 0
        let delim = if argc >= 2 then s 1 else " "
        let parts = str.Split([|delim|], StringSplitOptions.None)
        VArray (parts |> Array.map VString)
    | "join" ->
        match arg 0 with
        | VArray arr ->
            let delim = if argc >= 2 then s 1 else " "
            VString (arr |> Array.map valueToString |> String.concat delim)
        | _ -> VString (s 0)
    | "strcomp" ->
        let result = String.Compare(s 0, s 1, StringComparison.OrdinalIgnoreCase)
        VInteger (sign result)

    // Type conversion
    | "cint" | "int" -> VInteger (valueToInt (arg 0))
    | "clng" -> VInteger (valueToInt (arg 0))
    | "cdbl" -> VDouble (valueToDouble (arg 0))
    | "csng" -> VDouble (float (float32 (valueToDouble (arg 0))))
    | "cstr" -> VString (valueToString (arg 0))
    | "cbool" -> VBoolean (valueToBool (arg 0))
    | "cbyte" -> VInteger (valueToInt (arg 0) &&& 0xFF)
    | "fix" -> VInteger (int (Math.Truncate(valueToDouble (arg 0))))

    // Type checking
    | "isnull" -> VBoolean (match arg 0 with VNull -> true | _ -> false)
    | "isempty" -> VBoolean (match arg 0 with VEmpty -> true | _ -> false)
    | "isnumeric" ->
        match arg 0 with
        | VInteger _ | VDouble _ -> VBoolean true
        | VString s -> VBoolean (match Double.TryParse(s) with true, _ -> true | _ -> false)
        | _ -> VBoolean false
    | "isarray" -> VBoolean (match arg 0 with VArray _ -> true | _ -> false)
    | "isobject" -> VBoolean (match arg 0 with VObject _ | VNothing -> true | _ -> false)
    | "isdate" -> VBoolean false
    | "typename" ->
        let name =
            match arg 0 with
            | VInteger _ -> "Integer"
            | VDouble _ -> "Double"
            | VString _ -> "String"
            | VBoolean _ -> "Boolean"
            | VNull -> "Null"
            | VEmpty -> "Empty"
            | VNothing -> "Nothing"
            | VArray _ -> "Variant()"
            | VObject o -> o.ClassName
            | VUndefined -> "Undefined"
        VString name
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
            | _ -> 0
        VInteger vt

    // Math
    | "abs" ->
        match arg 0 with
        | VInteger i -> VInteger (abs i)
        | VDouble d -> VDouble (abs d)
        | _ -> VDouble (abs (valueToDouble (arg 0)))
    | "sgn" ->
        match arg 0 with
        | VInteger i -> VInteger (sign i)
        | _ -> VInteger (sign (valueToDouble (arg 0)))
    | "sqr" -> VDouble (sqrt (valueToDouble (arg 0)))
    | "exp" -> VDouble (Math.Exp(valueToDouble (arg 0)))
    | "log" -> VDouble (Math.Log(valueToDouble (arg 0)))
    | "sin" -> VDouble (Math.Sin(valueToDouble (arg 0)))
    | "cos" -> VDouble (Math.Cos(valueToDouble (arg 0)))
    | "tan" -> VDouble (Math.Tan(valueToDouble (arg 0)))
    | "atn" -> VDouble (Math.Atan(valueToDouble (arg 0)))
    | "rnd" -> VDouble (rng.NextDouble())
    | "round" ->
        let value = valueToDouble (arg 0)
        let decimals = if argc >= 2 then n 1 else 0
        VDouble (Math.Round(value, decimals, MidpointRounding.AwayFromZero))
    | "hex" -> VString (sprintf "%X" (valueToInt (arg 0)))
    | "oct" -> VString (Convert.ToString(valueToInt (arg 0), 8))

    // Array
    | "array" -> VArray args
    | "ubound" ->
        match arg 0 with
        | VArray arr -> VInteger (arr.Length - 1)
        | _ -> VInteger -1
    | "lbound" -> VInteger 0

    // Misc
    | "msgbox" ->
        printfn "%s" (s 0)
        VInteger 1
    | "inputbox" ->
        printf "%s" (s 0)
        let input = Console.ReadLine()
        VString (if input = null then "" else input)
    | "now" -> VString (DateTime.Now.ToString())
    | "timer" -> VDouble (float (DateTime.Now.TimeOfDay.TotalSeconds))
    | "formatnumber" ->
        let value = valueToDouble (arg 0)
        let decimals = if argc >= 2 then n 1 else 2
        VString (value.ToString("F" + string decimals))

    | _ -> VEmpty
