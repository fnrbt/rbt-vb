module Rbt.Vb.Builtins

open Values
open ValueSemantics
open System
open System.Text

let private compareMode defaultTextCompare value =
    match value with
    | VUndefined | VEmpty -> defaultTextCompare
    | _ ->
    match toInteger value with
    | 0 -> false
    | 1 -> true
    | -1 -> defaultTextCompare
    | _ -> raiseVbError 5

let builtinArray (data: Value array) =
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
    VLong (if upper then lower + array.Lengths.[index] - 1 else lower)

// ── Rnd: VBA's 24-bit generator, so seeded sequences match VBA ──

let mutable private rndSeed = 327680L

let private singleBits (x: float) =
    let bytes = BitConverter.GetBytes(float32 x)
    int64 bytes.[0] ||| (int64 bytes.[1] <<< 8) ||| (int64 bytes.[2] <<< 16) ||| (int64 bytes.[3] <<< 24)

let private doubleHighWord (x: float) =
    let bytes = BitConverter.GetBytes(x)
    int64 bytes.[4] ||| (int64 bytes.[5] <<< 8) ||| (int64 bytes.[6] <<< 16) ||| (int64 bytes.[7] <<< 24)

let rnd (number: float option) =
    let n = defaultArg number 1.0
    if n < 0.0 then
        let bits = singleBits n
        rndSeed <- (bits + (bits >>> 24)) &&& 0xFFFFFFL
    if n <> 0.0 then
        rndSeed <- (rndSeed * 0x43FD43FDL + 0xC39EC3L) &&& 0xFFFFFFL
    float rndSeed / 16777216.0

let randomize (seed: float) =
    let hi = doubleHighWord seed
    let mixed = (((hi &&& 0xFFFFL) ^^^ (hi >>> 16)) <<< 8) &&& 0xFFFF00L
    rndSeed <- (rndSeed &&& 0xFFL) ||| mixed

/// Text of a `Debug.Print x;` waiting for the rest of its line.
let mutable private pendingPrint = ""

/// Print a `Debug.Print x;` line that nothing completed (at the end of a run).
let flushPendingPrint (hooks: RuntimeHooks) =
    if pendingPrint <> "" then
        let text = pendingPrint
        pendingPrint <- ""
        hooks.Print text

let rec private typeName = function
    | VInteger n when n >= -32768 && n <= 32767 -> "Integer"
    | VInteger _ | VLong _ -> "Long"
    | VSingle _ -> "Single"
    | VByte _ -> "Byte"
    | VDouble _ -> "Double"
    | VString _ -> "String"
    | VBoolean _ -> "Boolean"
    | VDate _ -> "Date"
    | VCurrency _ -> "Currency"
    | VError _ -> "Error"
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

let varType value =
    match value with
    | VEmpty -> 0
    | VNull -> 1
    | VInteger n when n >= -32768 && n <= 32767 -> 2
    | VInteger _ | VLong _ -> 3
    | VSingle _ -> 4
    | VDouble _ -> 5
    | VByte _ -> 17
    | VCurrency _ -> 6
    | VDate _ -> 7
    | VString _ -> 8
    | VObject _ | VHostObject _ | VNothing -> 9
    | VError _ | VUndefined -> 10
    | VBoolean _ -> 11
    | VArray _ -> 8204
    | _ -> 0

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

// ── String searching with a compare mode ──

let private textEquals textMode (text: string) (index: int) (find: string) =
    if index < 0 || index + find.Length > text.Length then false
    else
        let part = text.Substring(index, find.Length)
        if textMode then part.ToUpperInvariant() = find.ToUpperInvariant() else part = find

let private indexOf textMode (text: string) (find: string) (start: int) =
    if start > text.Length then -1
    elif not textMode then text.IndexOf(find, start, StringComparison.Ordinal)
    else
#if FABLE_COMPILER
        text.ToUpperInvariant().IndexOf(find.ToUpperInvariant(), start)
#else
        text.IndexOf(find, start, StringComparison.OrdinalIgnoreCase)
#endif

/// The last match that fits in text.[0 .. limit - 1].
let private lastIndexOf textMode (text: string) (find: string) (limit: int) =
    let mutable position = min limit text.Length - find.Length
    let mutable found = -1
    while found < 0 && position >= 0 do
        if textEquals textMode text position find then found <- position
        position <- position - 1
    found

let replaceText textMode (text: string) (find: string) (replacement: string) (start: int) (count: int) =
    let text = if start > 1 then (if start - 1 >= text.Length then "" else text.Substring(start - 1)) else text
    if find = "" || count = 0 then text
    else
        let sb = StringBuilder()
        let mutable position = 0
        let mutable replaced = 0
        let mutable searching = true
        while searching do
            let index = if count >= 0 && replaced >= count then -1 else indexOf textMode text find position
            if index < 0 then
                sb.Append(text.Substring position) |> ignore
                searching <- false
            else
                sb.Append(text.Substring(position, index - position)).Append(replacement) |> ignore
                position <- index + find.Length
                replaced <- replaced + 1
        sb.ToString()

let splitText textMode (text: string) (delimiter: string) (limit: int) =
    if text = "" then [||]
    elif delimiter = "" || limit = 1 then [| text |]
    else
        let parts = ResizeArray<string>()
        let mutable position = 0
        let mutable searching = true
        while searching do
            let index = if limit > 0 && parts.Count >= limit - 1 then -1 else indexOf textMode text delimiter position
            if index < 0 then
                parts.Add(text.Substring position)
                searching <- false
            else
                parts.Add(text.Substring(position, index - position))
                position <- index + delimiter.Length
        parts.ToArray()

let private properCase (text: string) =
    let chars = text.ToLowerInvariant().ToCharArray()
    let mutable startOfWord = true
    for i in 0 .. chars.Length - 1 do
        if Char.IsLetter chars.[i] || Char.IsDigit chars.[i] || chars.[i] = '\'' then
            if startOfWord && Char.IsLetter chars.[i] then chars.[i] <- Char.ToUpperInvariant chars.[i]
            startOfWord <- false
        else startOfWord <- true
    String(chars)

let private hexDigits (value: int64) (radix: int) =
    if value = 0L then "0"
    else
        let digits = "0123456789ABCDEF"
        let mutable text = ""
        let mutable v = value
        while v > 0L do
            text <- string digits.[int (v % int64 radix)] + text
            v <- v / int64 radix
        text

/// Hex/Oct of a whole number: negative Integers (and Bytes and Booleans) use
/// 16 bits, Longs 32 and other types 64, as 64-bit VBA does.
let private radixText (value: Value) (radix: int) =
    match scalar value with
    | VNull -> VNull
    | VEmpty -> VString "0"
    | v ->
        let n =
            match v with
            | VInteger _ | VLong _ | VByte _ | VBoolean _ -> int64 (toInteger v)
            | other ->
                let x = Math.Round(toNumber other, 0, MidpointRounding.ToEven)
                if Double.IsNaN x || Math.Abs x >= 9.2233720368547758E18 then raiseVbError 6
                int64 x
        let text =
            if n >= 0L then hexDigits n radix
            else
                match v with
                | VInteger i when i >= -32768 -> hexDigits (n + 65536L) radix
                | VBoolean _ -> hexDigits (n + 65536L) radix
                | VInteger _ | VLong _ -> hexDigits (n + 4294967296L) radix
                | _ ->
                    // 64-bit two's complement, digit by digit.
                    let bits = if radix = 16 then 4 else 3
                    let mask = uint64 (radix - 1)
                    let digits = "0123456789ABCDEF"
                    let mutable u = uint64 n
                    let mutable out = ""
                    let mutable count = 0
                    while count < 64 do
                        out <- string digits.[int (u &&& mask)] + out
                        u <- u >>> bits
                        count <- count + bits
                    if radix = 8 then
                        // 64 bits are 22 octal digits; the top digit holds one bit.
                        out.TrimStart('0')
                    else out
        VString text

let private lenOf (value: Value) = (toString value).Length

// ── Dates ──

let private nowSerial () =
    let now = DateTime.Now
    DateCore.fromComponents now.Year now.Month now.Day (float (now.Hour * 3600 + now.Minute * 60 + now.Second))

let private dateArg (value: Value) =
    match scalar value with
    | VNull -> None
    | v -> Some (toDate v)

let private firstDayArg (value: Value) =
    match value with
    | VUndefined | VEmpty -> 1
    | v ->
        let day = toInteger v
        if day < 0 || day > 7 then raiseVbError 5
        if day = 0 then 1 else day

let private firstWeekArg (value: Value) =
    match value with
    | VUndefined | VEmpty -> 1
    | v ->
        let week = toInteger v
        if week < 0 || week > 3 then raiseVbError 5
        if week = 0 then 1 else week

let private intervalOf (value: Value) = (toString value).Trim().ToLowerInvariant()

let private checkedDateValue (serial: float) =
    if not (DateCore.isValidSerial serial) then raiseVbError 5
    VDate serial

let dateAdd (interval: string) (number: float) (serial: float) =
    let n = Math.Truncate number
    let addMonths months =
        let y, m, d, h, mi, s = DateCore.components serial
        let total = y * 12 + (m - 1) + int months
        let ny = (if total >= 0 then total / 12 else (total - 11) / 12)
        let nm = total - ny * 12 + 1
        if ny < 100 || ny > 9999 then raiseVbError 5
        let nd = min d (DateCore.daysInMonth ny nm)
        DateCore.fromComponents ny nm nd (float (h * 3600 + mi * 60 + s))
    let addLinear days = DateCore.ofLinear (DateCore.toLinear serial + days)
    let result =
        match interval with
        | "yyyy" -> addMonths (n * 12.0)
        | "q" -> addMonths (n * 3.0)
        | "m" -> addMonths n
        | "y" | "d" | "w" -> addLinear n
        | "ww" -> addLinear (n * 7.0)
        | "h" -> addLinear (n / 24.0)
        | "n" -> addLinear (n / 1440.0)
        | "s" -> addLinear (n / 86400.0)
        | _ -> raiseVbError 5
    checkedDateValue result

let private totalSeconds (serial: float) = int64 (DateCore.datePart serial) * 86400L + int64 (DateCore.timeSeconds serial)

let private floorDiv64 (a: int64) (b: int64) = if a >= 0L then a / b else (a - b + 1L) / b

let dateDiff (interval: string) (first: float) (second: float) (firstDay: int) (firstWeek: int) =
    let y1, m1, _, _, _, _ = DateCore.components first
    let y2, m2, _, _, _, _ = DateCore.components second
    let day1 = int64 (DateCore.datePart first)
    let day2 = int64 (DateCore.datePart second)
    let weekStart (serial: float) day = day - int64 (DateCore.weekday serial firstDay - 1)
    ignore firstWeek
    let result =
        match interval with
        | "yyyy" -> int64 (y2 - y1)
        | "q" -> int64 ((y2 * 4 + (m2 - 1) / 3) - (y1 * 4 + (m1 - 1) / 3))
        | "m" -> int64 ((y2 * 12 + m2) - (y1 * 12 + m1))
        | "y" | "d" -> day2 - day1
        | "w" -> (day2 - day1) / 7L
        | "ww" -> (weekStart second day2 - weekStart first day1) / 7L
        | "h" -> floorDiv64 (totalSeconds second) 3600L - floorDiv64 (totalSeconds first) 3600L
        | "n" -> floorDiv64 (totalSeconds second) 60L - floorDiv64 (totalSeconds first) 60L
        | "s" -> totalSeconds second - totalSeconds first
        | _ -> raiseVbError 5
    // A Long; a difference beyond the Long range is a Double.
    if result > 2147483647L || result < -2147483648L then VDouble (float result)
    else VLong (int result)

let datePart (interval: string) (serial: float) (firstDay: int) (firstWeek: int) =
    let y, m, d, h, n, s = DateCore.components serial
    let value =
        match interval with
        | "yyyy" -> y
        | "q" -> (m - 1) / 3 + 1
        | "m" -> m
        | "y" -> DateCore.datePart serial - DateCore.dayNumber y 1 1 + 1
        | "d" -> d
        | "w" -> DateCore.weekday serial firstDay
        | "ww" -> DateCore.weekOfYear serial firstDay firstWeek
        | "h" -> h
        | "n" -> n
        | "s" -> s
        | _ -> raiseVbError 5
    VInteger value

let callBuiltin (hooks: RuntimeHooks) (defaultTextCompare: bool) (name: string) (args: Value array) : Value =
    let argc = args.Length
    let objectAware =
        match name with
        | "typename" | "vartype" | "isobject" | "array" | "ismissing" | "isarray" | "iif" | "choose" | "switch" | "cvar" | "iserror" -> true
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
            | VObject _ as o when not objectAware -> scalar o
            | v -> v
        else VEmpty
    let has i = i < argc && (match dereference args.[i] with VUndefined -> false | _ -> true)
    let raw i = if i < argc then dereference args.[i] else VUndefined
    let s i = toString (arg i)
    let n i = toInteger (arg i)
    let d i = toNumber (arg i)
    let isNull i = match arg i with VNull -> true | _ -> false
    let textMode i = if has i then compareMode defaultTextCompare (arg i) else defaultTextCompare
    let stringOrNull i (f: string -> string) = if isNull i then VNull else VString (f (s i))
    let dateOrNull i (f: float -> Value) = match dateArg (arg i) with Some serial -> f serial | None -> VNull

    match name with
    // Output
    | "print" ->
        let sb = StringBuilder()
        for i in 0 .. argc - 1 do
            if i > 0 then sb.Append " " |> ignore
            sb.Append(printable args.[i]) |> ignore
        hooks.Print (sb.ToString())
        VEmpty
    | "debug.print" | "debug.print;" ->
        // Numbers print with a sign position and a trailing space, as in VBA.
        // A trailing ';' or ',' keeps the text for the next Debug.Print.
        let sb = StringBuilder(pendingPrint)
        pendingPrint <- ""
        for i in 0 .. argc - 1 do
            match scalar args.[i] with
            | VInteger number | VLong number -> sb.Append((if number >= 0 then " " else "") + string number + " ") |> ignore
            | VByte number -> sb.Append(" " + string number + " ") |> ignore
            | (VDouble _ | VCurrency _ | VSingle _) as number ->
                let text = toString number
                sb.Append((if text.StartsWith "-" then "" else " ") + text + " ") |> ignore
            | VBoolean b -> sb.Append(if b then "True" else "False") |> ignore
            | _ -> sb.Append(printable args.[i]) |> ignore
        if name = "debug.print;" then pendingPrint <- sb.ToString()
        else hooks.Print (sb.ToString())
        VEmpty
    | "for:start" -> forCounterStart (raw 0) (raw 1) (raw 2)
    | "ismissing" ->
        argCount args 1 1
        VBoolean (match dereference args.[0] with VUndefined -> true | _ -> false)
    | "rgb" ->
        argCount args 3 3
        let channel i = max 0 (min 255 (n i))
        VLong (channel 0 + channel 1 * 256 + channel 2 * 65536)
    | "qbcolor" ->
        argCount args 1 1
        let colors = [| 0x000000; 0x800000; 0x008000; 0x808000; 0x000080; 0x800080; 0x008080; 0xC0C0C0
                        0x808080; 0xFF0000; 0x00FF00; 0xFFFF00; 0x0000FF; 0xFF00FF; 0x00FFFF; 0xFFFFFF |]
        let index = n 0
        if index < 0 || index > 15 then raiseVbError 5
        VLong colors.[index]
    | "createobject" ->
        argCount args 1 2
        let progId = s 0
        match Collections.tryCreate progId with
        | Some value -> value
        | None ->
            match hooks.CreateObject progId with
            | Some value -> value
            | None -> raiseVbErrorWith 429 (errorDescription 429 + ": " + progId)
    | "getobject" -> raiseVbError 429
    | "new:builtin" ->
        match Collections.tryNew (s 0) with
        | Some value -> value
        | None -> raiseVbError 429
    | "environ" ->
        argCount args 1 1
        match arg 0 with
        | VString variable -> VString (hooks.Environ variable)
        | _ -> VString ""
    | "doevents" -> HostSignals.requestYield (); VInteger 0
    | "beep" -> VEmpty

    // String functions
    | "len" -> if isNull 0 then VNull else VLong (lenOf (arg 0))
    | "lenb" -> if isNull 0 then VNull else VLong (2 * lenOf (arg 0))
    | "left" ->
        argCount args 2 2
        let count = n 1
        if count < 0 then raiseVbError 5
        stringOrNull 0 (fun str -> str.Substring(0, min count str.Length))
    | "right" ->
        argCount args 2 2
        let count = n 1
        if count < 0 then raiseVbError 5
        stringOrNull 0 (fun str -> str.Substring(str.Length - min count str.Length))
    | "mid" ->
        argCount args 2 3
        let start = n 1
        if start < 1 then raiseVbError 5
        let length = if has 2 then n 2 else Int32.MaxValue
        if length < 0 then raiseVbError 5
        stringOrNull 0 (fun str ->
            if start > str.Length then ""
            else str.Substring(start - 1, min length (str.Length - start + 1)))
    | "mid:statement" ->
        // Mid(target, start, length) = replacement: the new value of target.
        let target = s 0
        let start = n 1
        if start < 1 || start > target.Length + (if target.Length = 0 then 1 else 0) then raiseVbError 5
        let replacement = s 3
        let length = if has 2 then min (n 2) replacement.Length else replacement.Length
        let length = min length (target.Length - start + 1)
        if length < 0 then raiseVbError 5
        VString (target.Substring(0, start - 1) + replacement.Substring(0, length) + target.Substring(start - 1 + length))
    | "lset:statement" ->
        let target = s 0
        let value = s 1
        VString (if value.Length >= target.Length then value.Substring(0, target.Length) else value + String(' ', target.Length - value.Length))
    | "rset:statement" ->
        let target = s 0
        let value = s 1
        VString (if value.Length >= target.Length then value.Substring(0, target.Length) else String(' ', target.Length - value.Length) + value)
    | "ucase" -> stringOrNull 0 (fun str -> str.ToUpperInvariant())
    | "lcase" -> stringOrNull 0 (fun str -> str.ToLowerInvariant())
    | "trim" -> stringOrNull 0 (fun str -> str.Trim(' '))
    | "ltrim" -> stringOrNull 0 (fun str -> str.TrimStart(' '))
    | "rtrim" -> stringOrNull 0 (fun str -> str.TrimEnd(' '))
    | "instr" ->
        if argc < 2 || argc > 4 then raiseVbError 450
        let numericStart = argc >= 3
        let first, second = if numericStart then arg 1, arg 2 else arg 0, arg 1
        match first, second with
        | VNull, _ | _, VNull -> VNull
        | _ ->
            let start, text, needle, mode =
                if not numericStart then 1, s 0, s 1, defaultTextCompare
                else (if has 0 then n 0 else 1), s 1, s 2, textMode 3
            if start < 1 then raiseVbError 5
            if start > text.Length then VLong 0
            elif needle.Length = 0 then VLong start
            else
                let index = indexOf mode text needle (start - 1)
                VLong (if index < 0 then 0 else index + 1)
    | "instrrev" ->
        argCount args 2 4
        match arg 0, arg 1 with
        | VNull, _ | _, VNull -> VNull
        | _ ->
            let text = s 0
            let find = s 1
            let start = if has 2 then n 2 else -1
            if start = 0 || start < -1 then raiseVbError 5
            let start = if start = -1 then text.Length else start
            if text.Length = 0 || start > text.Length then VLong 0
            elif find.Length = 0 then VLong start
            else
                let index = lastIndexOf (textMode 3) text find start
                VLong (if index < 0 then 0 else index + 1)
    | "replace" ->
        argCount args 3 6
        if isNull 0 then VNull
        else
            let start = if has 3 then n 3 else 1
            let count = if has 4 then n 4 else -1
            if start < 1 || count < -1 then raiseVbError 5
            VString (replaceText (textMode 5) (s 0) (s 1) (s 2) start count)
    | "space" ->
        let count = n 0
        if count < 0 then raiseVbError 5
        VString (String(' ', count))
    | "string" ->
        argCount args 2 2
        let count = n 0
        if count < 0 then raiseVbError 5
        let ch =
            match arg 1 with
            | VString str when str.Length > 0 -> str.[0]
            | VString _ -> raiseVbError 5
            | VNull -> raiseVbError 94
            | other -> char ((toInteger other % 256 + 256) % 256)
        VString (String(ch, count))
    | "strreverse" -> VString (String(Array.rev ((s 0).ToCharArray())))
    | "asc" | "ascw" | "ascb" ->
        let str = s 0
        if str.Length = 0 then raiseVbError 5
        let code = int str.[0]
        match name with
        | "ascb" -> VByte (byte (code &&& 255))
        | "ascw" -> VInteger (if code > 32767 then code - 65536 else code)
        | _ -> VInteger (if code > 32767 then code - 65536 else code)
    | "chr" | "chrb" ->
        let code = n 0
        if code < -32768 || code > 65535 then raiseVbError 5
        VString (string (char (if code < 0 then code + 65536 else code)))
    | "chrw" ->
        let code = n 0
        if code < -32768 || code > 65535 then raiseVbError 5
        VString (string (char (if code < 0 then code + 65536 else code)))
    | "strconv" ->
        argCount args 2 3
        if isNull 0 then VNull
        else
            let str = s 0
            match n 1 &&& 3 with
            | 1 -> VString (str.ToUpperInvariant())
            | 2 -> VString (str.ToLowerInvariant())
            | 3 -> VString (properCase str)
            | _ -> VString str
    | "split" ->
        argCount args 1 4
        let text = s 0
        let delim = if has 1 then s 1 else " "
        let limit = if has 2 then n 2 else -1
        if limit < -1 then raiseVbError 5
        builtinArray (splitText (textMode 3) text delim limit |> Array.map VString)
    | "join" ->
        argCount args 1 2
        match arg 0 with
        | VArray arr when arr.Lengths.Length = 1 ->
            let delim = if has 1 then s 1 else " "
            VString (arr.Data |> Array.map toString |> String.concat delim)
        | VArray arr when arr.Lengths.Length = 0 -> VString ""
        | _ -> raiseVbError 13
    | "filter" ->
        argCount args 2 4
        match arg 0 with
        | VArray arr when arr.Lengths.Length = 1 ->
            let find = s 1
            let keep = if has 2 then toBool (arg 2) else true
            let mode = textMode 3
            let kept =
                arr.Data |> Array.filter (fun item ->
                    let text = toString item
                    (indexOf mode text find 0 >= 0) = keep)
            builtinArray (kept |> Array.map (fun item -> VString (toString item)))
        | VNull -> raiseVbError 94
        | _ -> raiseVbError 13
    | "strcomp" ->
        if argc < 2 || argc > 3 then raiseVbError 450
        match arg 0, arg 1 with
        | VNull, _ | _, VNull -> VNull
        | _ -> VInteger (sign (compareStrings (textMode 2) (s 0) (s 1)))
    | "format" ->
        argCount args 1 4
        let fmt = if has 1 then s 1 else ""
        Formatting.format (arg 0) fmt (firstDayArg (raw 2)) (firstWeekArg (raw 3))
    | "formatnumber" | "formatcurrency" | "formatpercent" ->
        argCount args 1 5
        if isNull 0 then raiseVbError 13
        let opt i fallback = if has i then n i else fallback
        let value = arg 0
        let value = match value with VString _ | VBoolean _ | VEmpty -> VDouble (toNumber value) | v -> v
        let decimals = opt 1 -1
        match name with
        | "formatnumber" -> VString (Formatting.formatFixed value decimals (opt 2 -2) (opt 3 -2) (opt 4 -2) false "" "")
        | "formatcurrency" -> VString (Formatting.formatFixed value decimals (opt 2 -2) (opt 3 -2) (opt 4 -2) true "$" "")
        | _ -> VString (Formatting.formatFixed (multiplyValues value (VInteger 100)) decimals (opt 2 -2) (opt 3 -2) (opt 4 -2) false "" "%")
    | "formatdatetime" ->
        argCount args 1 2
        match dateArg (arg 0) with
        | Some serial -> VString (Formatting.formatDateTime serial (if has 1 then n 1 else 0))
        | None -> raiseVbError 13
    | "str" -> argCount args 1 1; Formatting.str (arg 0)
    | "val" -> argCount args 1 1; VDouble (Formatting.valOf (s 0))

    // Type conversion
    | "cint" -> VInteger (roundedInteger -32768.0 32767.0 (arg 0))
    | "clng" -> VLong (toInteger (arg 0))
    | "clnglng" | "clngptr" -> toLongLong (arg 0)
    | "int" -> floorValue (arg 0)
    | "fix" -> truncateValue (arg 0)
    | "cdbl" | "cdec" -> VDouble (d 0)
    | "csng" -> toSingleValue (arg 0)
    | "ccur" -> VCurrency (toCurrencyUnits (arg 0))
    | "cdate" | "cvdate" -> VDate (toDate (arg 0))
    | "cstr" -> VString (s 0)
    | "cbool" -> VBoolean (toBool (arg 0))
    | "cbyte" -> VByte (byte (roundedInteger 0.0 255.0 (arg 0)))
    | "cvar" -> arg 0
    | "cverr" -> argCount args 1 1; VError (n 0)

    // Type checking
    | "isnull" -> VBoolean (match arg 0 with VNull -> true | _ -> false)
    | "isempty" -> VBoolean (match arg 0 with VEmpty -> true | _ -> false)
    | "isnumeric" ->
        match arg 0 with
        | VInteger _ | VLong _ | VSingle _ | VByte _ | VDouble _ | VCurrency _ | VBoolean _ | VEmpty -> VBoolean true
        | VString text -> VBoolean (tryParseNumber text).IsSome
        | _ -> VBoolean false
    | "isarray" -> VBoolean (match arg 0 with VArray _ -> true | _ -> false)
    | "isobject" -> VBoolean (match arg 0 with VObject _ | VHostObject _ | VNothing -> true | _ -> false)
    | "isdate" ->
        match arg 0 with
        | VDate _ -> VBoolean true
        | VString text -> VBoolean (tryParseDate text).IsSome
        | _ -> VBoolean false
    | "iserror" -> VBoolean (match raw 0 with VError _ | VUndefined -> true | _ -> false)
    | "typename" ->
        VString (typeName (arg 0))
    | "vartype" -> VLong (varType (arg 0))

    // Choices
    | "iif" ->
        argCount args 3 3
        let condition = match scalar (raw 0) with VNull -> false | v -> toBool v
        if condition then raw 1 else raw 2
    | "choose" ->
        if argc < 1 then raiseVbError 449
        match scalar (raw 0) with
        | VNull -> raiseVbError 94
        | index ->
            let i = int (Math.Round(toNumber index, 0, MidpointRounding.ToEven))
            if i >= 1 && i < argc then raw i else VNull
    | "switch" ->
        if argc % 2 <> 0 then raiseVbError 5
        let mutable result = VNull
        let mutable i = 0
        while i < argc do
            let condition = match scalar (raw i) with VNull -> false | v -> toBool v
            if condition then
                result <- raw (i + 1)
                i <- argc
            else i <- i + 2
        result

    // Math
    | "abs" -> absValue (arg 0)
    | "sgn" ->
        match arg 0 with
        | VNull -> VNull
        | VCurrency c -> VInteger (sign c)
        | v -> VInteger (sign (toNumber v))
    | "sqr" ->
        let x = d 0
        if x < 0.0 then raiseVbError 5
        VDouble (sqrt x)
    | "exp" -> VDouble (Math.Exp(d 0))
    | "log" ->
        let x = d 0
        if x <= 0.0 then raiseVbError 5
        VDouble (Math.Log x)
    | "sin" -> VDouble (Math.Sin(d 0))
    | "cos" -> VDouble (Math.Cos(d 0))
    | "tan" -> VDouble (Math.Tan(d 0))
    | "atn" -> VDouble (Math.Atan(d 0))
    | "rnd" -> single (rnd (if has 0 then Some (d 0) else None))
    | "randomize" ->
        randomize (if has 0 then d 0 else float DateTime.Now.TimeOfDay.TotalSeconds)
        VEmpty
    | "round" ->
        argCount args 1 2
        let decimals = if has 1 then n 1 else 0
        if decimals < 0 || decimals > 22 then raiseVbError 5
        match arg 0 with
        | VNull -> VNull
        | (VInteger _ | VLong _ | VByte _) as whole -> whole
        | VBoolean b -> VInteger (if b then -1 else 0)
        | VSingle f -> VSingle (toSingle (Math.Round(float f, min decimals 15, MidpointRounding.ToEven)))
        | VCurrency c ->
            if decimals >= 4 then VCurrency c
            else
                let unit = match decimals with 0 -> 10000L | 1 -> 1000L | 2 -> 100L | _ -> 10L
                // Round half to even in whole units.
                let q = c / unit
                let twice = 2L * abs (c - q * unit)
                let q = if twice > unit || (twice = unit && q % 2L <> 0L) then q + (if c < 0L then -1L else 1L) else q
                VCurrency (q * unit)
        | VDate serial -> VDate (Math.Round(serial, min decimals 15, MidpointRounding.ToEven))
        | value -> VDouble (Math.Round(toNumber value, min decimals 15, MidpointRounding.ToEven))
    | "hex" -> radixText (arg 0) 16
    | "oct" -> radixText (arg 0) 8

    // Dates
    | "now" -> VDate (nowSerial ())
    | "date" -> VDate (Math.Floor (nowSerial ()))
    | "time" -> VDate (let now = nowSerial () in now - Math.Floor now)
    | "timer" -> single DateTime.Now.TimeOfDay.TotalSeconds
    | "year" -> dateOrNull 0 (fun serial -> let y, _, _, _, _, _ = DateCore.components serial in VInteger y)
    | "month" -> dateOrNull 0 (fun serial -> let _, m, _, _, _, _ = DateCore.components serial in VInteger m)
    | "day" -> dateOrNull 0 (fun serial -> let _, _, day, _, _, _ = DateCore.components serial in VInteger day)
    | "hour" -> dateOrNull 0 (fun serial -> let _, _, _, h, _, _ = DateCore.components serial in VInteger h)
    | "minute" -> dateOrNull 0 (fun serial -> let _, _, _, _, mi, _ = DateCore.components serial in VInteger mi)
    | "second" -> dateOrNull 0 (fun serial -> let _, _, _, _, _, sec = DateCore.components serial in VInteger sec)
    | "weekday" ->
        argCount args 1 2
        dateOrNull 0 (fun serial -> VInteger (DateCore.weekday serial (firstDayArg (raw 1))))
    | "weekdayname" ->
        argCount args 1 3
        let index = n 0
        if index < 1 || index > 7 then raiseVbError 5
        let first = firstDayArg (raw 2)
        let dayName = DateCore.dayNames.[(index - 1 + first - 1) % 7]
        VString (if has 1 && toBool (arg 1) then DateCore.abbreviation dayName else dayName)
    | "monthname" ->
        argCount args 1 2
        let index = n 0
        if index < 1 || index > 12 then raiseVbError 5
        let monthName = DateCore.monthNames.[index - 1]
        VString (if has 1 && toBool (arg 1) then DateCore.abbreviation monthName else monthName)
    | "dateserial" ->
        argCount args 3 3
        checkedDateValue (DateCore.dateSerial (n 0) (n 1) (n 2))
    | "timeserial" ->
        argCount args 3 3
        let seconds = float (n 0) * 3600.0 + float (n 1) * 60.0 + float (n 2)
        checkedDateValue (DateCore.ofLinear (seconds / 86400.0))
    | "datevalue" ->
        argCount args 1 1
        dateOrNull 0 (fun serial -> VDate (float (DateCore.datePart serial)))
    | "timevalue" ->
        argCount args 1 1
        dateOrNull 0 (fun serial -> VDate (float (DateCore.timeSeconds serial) / 86400.0))
    | "dateadd" ->
        argCount args 3 3
        dateOrNull 2 (fun serial -> dateAdd (intervalOf (arg 0)) (d 1) serial)
    | "datediff" ->
        argCount args 3 5
        match dateArg (arg 1), dateArg (arg 2) with
        | Some first, Some second -> dateDiff (intervalOf (arg 0)) first second (firstDayArg (raw 3)) (firstWeekArg (raw 4))
        | _ -> VNull
    | "datepart" ->
        argCount args 2 4
        dateOrNull 1 (fun serial -> datePart (intervalOf (arg 0)) serial (firstDayArg (raw 2)) (firstWeekArg (raw 3)))

    // Array
    | "array" -> builtinArray (Array.map dereference args)
    | "array:unallocated" -> VArray { Data = [||]; LowerBounds = [||]; Lengths = [||] }
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
        VLong (hooks.MsgBox (s 0) buttons title)
    | "inputbox" ->
        argCount args 1 7
        let title = if has 1 then s 1 else ""
        let defaultText = if has 2 then s 2 else ""
        VString (hooks.InputBox (s 0) title defaultText |> Option.defaultValue "")

    | _ -> raiseVbErrorWith 35 (errorDescription 35 + ": " + name)

/// Built-ins that may be called without parentheses or arguments as values
/// (`t = Now`, `x = Rnd`).
let isArgumentlessBuiltin (name: string) =
    match name.ToLowerInvariant() with
    | "now" | "date" | "time" | "timer" | "rnd" | "randomize" | "doevents" | "beep" -> true
    | _ -> false

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
    Environ = fun _ -> ""
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
        "vbUseSystemDayOfWeek", 0; "vbUseSystem", 0; "vbFirstJan1", 1; "vbFirstFourDays", 2; "vbFirstFullWeek", 3
        "vbGeneralDate", 0; "vbLongDate", 1; "vbShortDate", 2; "vbLongTime", 3; "vbShortTime", 4
        "vbUpperCase", 1; "vbLowerCase", 2; "vbProperCase", 3; "vbWide", 4; "vbNarrow", 8
        "vbKatakana", 16; "vbHiragana", 32; "vbUnicode", 64; "vbFromUnicode", 128
        "vbMethod", 1; "vbGet", 2; "vbLet", 4; "vbSet", 8 ] do
        add name (VLong number)
    table

/// The value of a built-in `vb*` constant (case-insensitive).
let tryConstant (name: string) =
    match vbConstants.TryGetValue(name.ToLowerInvariant()) with
    | true, value -> Some value
    | _ -> None
