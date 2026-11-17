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
    | _ -> raiseVbError 5

let private builtinArray (data: Value array) =
    VArray { Data = data; LowerBounds = [| 0 |]; Lengths = [| data.Length |] }

let private arrayBound upper (args: Value array) =
    let array =
        match dereference args.[0] with
        | VArray array -> array
        | _ -> raiseVbError 13
    let dimension = if args.Length >= 2 then toInteger args.[1] else 1
    if dimension < 1 || dimension > array.Lengths.Length then raiseVbError 9
    let index = dimension - 1
    let lower = array.LowerBounds.[index]
    VInteger (if upper then lower + array.Lengths.[index] - 1 else lower)
let private rng = Random()

let rec private typeName = function
    | VInteger n when n >= -32768 && n <= 32767 -> "Integer"
    | VInteger _ -> "Long"
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
    | VUndefined -> "Error"

let private argCount (args: Value array) minimum maximum =
    if args.Length < minimum || args.Length > maximum then raiseVbError 450

let private missingAsEmpty value =
    match value with
    | VUndefined -> VEmpty
    | other -> other

let private printable value =
    match scalar value with
    | VNull -> "Null"
    | VUndefined -> "Error 448"
    | other -> toString other

let callBuiltin (hooks: RuntimeHooks) (defaultTextCompare: bool) (name: string) (args: Value array) : Value =
    let argc = args.Length
    let objectAware =
        match name with
        | "typename" | "vartype" | "isobject" | "array" | "ismissing" | "isarray" -> true
        | _ -> false
    let arg i =
        if i < argc then
            match missingAsEmpty (dereference args.[i]) with
            | VHostObject host when not objectAware ->
                // Let-coercion through the default property, when there is one.
                match host.GetMember "" with
                | Some v -> scalar v
                | None ->
                    match host.GetMember "_Default" with
                    | Some v -> scalar v
                    | None ->
                        match host.GetMember "Value" with
                        | Some v -> scalar v
                        | None -> VHostObject host
            | v -> v
        else VEmpty
    let has i = i < argc && (match dereference args.[i] with VUndefined -> false | _ -> true)
    let s i = toString (arg i)
    let n i = toInteger (arg i)
    let d i = toNumber (arg i)

    match name with
    // Output
    | "print" ->
        let sb = StringBuilder()
        for i in 0 .. argc - 1 do
            if i > 0 then sb.Append " " |> ignore
            sb.Append(printable args.[i]) |> ignore
        hooks.Print (sb.ToString())
        VEmpty
    | "debug.print" ->
        // Numbers print with a sign position and a trailing space, as in VBA.
        let sb = StringBuilder()
        for i in 0 .. argc - 1 do
            match scalar args.[i] with
            | VInteger number -> sb.Append((if number >= 0 then " " else "") + string number + " ") |> ignore
            | VDouble number -> sb.Append((if number >= 0.0 then " " else "") + toString (VDouble number) + " ") |> ignore
            | _ -> sb.Append(printable args.[i]) |> ignore
        hooks.Print (sb.ToString())
        VEmpty
    | "ismissing" ->
        argCount args 1 1
        VBoolean (match dereference args.[0] with VUndefined -> true | _ -> false)
    | "erl" -> VInteger 0
    | "rgb" ->
        argCount args 3 3
        let channel i = max 0 (min 255 (n i))
        VInteger (channel 0 + channel 1 * 256 + channel 2 * 65536)
    | "qbcolor" ->
        argCount args 1 1
        let colors = [| 0x000000; 0x800000; 0x008000; 0x808000; 0x000080; 0x800080; 0x008080; 0xC0C0C0
                        0x808080; 0xFF0000; 0x00FF00; 0xFFFF00; 0x0000FF; 0xFF00FF; 0x00FFFF; 0xFFFFFF |]
        let index = n 0
        if index < 0 || index > 15 then raiseVbError 5
        VInteger colors.[index]
    | "createobject" ->
        argCount args 1 2
        let progId = s 0
        match hooks.CreateObject progId with
        | Some value -> value
        | None -> raiseVbErrorWith 429 (errorDescription 429 + ": " + progId)

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
        if argc <> 2 && argc <> 3 && argc <> 4 then raiseVbError 450
        let first, second = if argc = 2 then arg 0, arg 1 else arg 1, arg 2
        match first, second with
        | VNull, _ | _, VNull -> VNull
        | _ ->
            let start, text, needle, textMode =
                if argc = 2 then 1, s 0, s 1, defaultTextCompare
                else n 0, s 1, s 2, (if argc = 4 then compareMode defaultTextCompare (arg 3) else defaultTextCompare)
            if start < 1 then raiseVbError 5
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
        | _ -> raiseVbError 13
    | "strcomp" ->
        if argc < 2 || argc > 3 then raiseVbError 450
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
            | VInteger n when n >= -32768 && n <= 32767 -> 2
            | VInteger _ -> 3
            | VDouble _ -> 5
            | VString _ -> 8
            | VBoolean _ -> 11
            | VArray _ -> 0x2000
            | VObject _ -> 9
            | VHostObject _ -> 9
            | VNothing -> 9
            | VUndefined -> 10
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
        if decimals < 0 || decimals > 15 then raiseVbError 5
        VDouble (Math.Round(value, decimals, MidpointRounding.ToEven))
    | "hex" -> VString (sprintf "%X" (n 0))
    | "oct" -> VString (Convert.ToString(n 0, 8))

    // Array
    | "array" -> builtinArray args
    | "ubound" -> 
        argCount args 1 2
        arrayBound true args
    | "lbound" ->
        argCount args 1 2
        arrayBound false args

    // Misc
    | "msgbox" ->
        argCount args 1 5
        let buttons = if has 1 then n 1 else 0
        let title = if has 2 then s 2 else ""
        VInteger (hooks.MsgBox (s 0) buttons title)
    | "inputbox" ->
        argCount args 1 7
        let title = if has 1 then s 1 else ""
        let defaultText = if has 2 then s 2 else ""
        VString (hooks.InputBox (s 0) title defaultText |> Option.defaultValue "")
    | "now" -> VString (DateTime.Now.ToString())
    | "timer" -> VDouble (float (DateTime.Now.TimeOfDay.TotalSeconds))
    | "formatnumber" ->
        let value = d 0
        let decimals = if argc >= 2 then n 1 else 2
        VString (value.ToString("F" + string decimals))

    | _ -> raiseVbErrorWith 35 (errorDescription 35 + ": " + name)

/// Console behaviour used when the host supplies no hooks.
let consoleHooks : RuntimeHooks = {
    Print = fun text -> printfn "%s" text
    MsgBox = fun prompt _ _ -> printfn "%s" prompt; 1
    InputBox = fun prompt _ defaultText ->
        printf "%s" prompt
#if FABLE_COMPILER
        Some defaultText
#else
        match Console.ReadLine() with
        | null -> None
        | input -> Some input
#endif
    CreateObject = fun _ -> None
}

let private vbConstants =
    let table = System.Collections.Generic.Dictionary<string, Value>()
    let add (name: string) value = table.[name.ToLowerInvariant()] <- value
    for name, text in [ "vbCrLf", "\r\n"; "vbNewLine", "\r\n"; "vbCr", "\r"; "vbLf", "\n"; "vbTab", "\t"
                        "vbNullString", ""; "vbNullChar", "\000"; "vbBack", "\b"; "vbFormFeed", "\012"; "vbVerticalTab", "\011" ] do
        add name (VString text)
    for name, number in [
        "vbTrue", -1; "vbFalse", 0; "vbUseDefault", -2
        "vbBinaryCompare", 0; "vbTextCompare", 1; "vbDatabaseCompare", 2
        "vbObjectError", -2147221504
        "vbEmpty", 0; "vbNull", 1; "vbInteger", 2; "vbLong", 3; "vbSingle", 4; "vbDouble", 5; "vbCurrency", 6
        "vbDate", 7; "vbString", 8; "vbObject", 9; "vbError", 10; "vbBoolean", 11; "vbVariant", 12
        "vbDataObject", 13; "vbDecimal", 14; "vbByte", 17; "vbLongLong", 20; "vbUserDefinedType", 36; "vbArray", 8192
        "vbOKOnly", 0; "vbOKCancel", 1; "vbAbortRetryIgnore", 2; "vbYesNoCancel", 3; "vbYesNo", 4; "vbRetryCancel", 5
        "vbCritical", 16; "vbQuestion", 32; "vbExclamation", 48; "vbInformation", 64
        "vbDefaultButton1", 0; "vbDefaultButton2", 256; "vbDefaultButton3", 512; "vbDefaultButton4", 768
        "vbApplicationModal", 0; "vbSystemModal", 4096; "vbMsgBoxHelpButton", 16384
        "vbMsgBoxSetForeground", 65536; "vbMsgBoxRight", 524288; "vbMsgBoxRtlReading", 1048576
        "vbOK", 1; "vbCancel", 2; "vbAbort", 3; "vbRetry", 4; "vbIgnore", 5; "vbYes", 6; "vbNo", 7
        "vbBlack", 0; "vbRed", 255; "vbGreen", 65280; "vbYellow", 65535; "vbBlue", 16711680
        "vbMagenta", 16711935; "vbCyan", 16776960; "vbWhite", 16777215
        "vbSunday", 1; "vbMonday", 2; "vbTuesday", 3; "vbWednesday", 4; "vbThursday", 5; "vbFriday", 6; "vbSaturday", 7
        "vbUseSystemDayOfWeek", 0; "vbFirstJan1", 1; "vbFirstFourDays", 2; "vbFirstFullWeek", 3
        "vbGeneralDate", 0; "vbLongDate", 1; "vbShortDate", 2; "vbLongTime", 3; "vbShortTime", 4
        "vbUpperCase", 1; "vbLowerCase", 2; "vbProperCase", 3 ] do
        add name (VInteger number)
    table

/// The value of a built-in `vb*` constant (case-insensitive).
let tryConstant (name: string) =
    match vbConstants.TryGetValue(name.ToLowerInvariant()) with
    | true, value -> Some value
    | _ -> None
