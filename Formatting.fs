/// VB's Format, FormatNumber, FormatCurrency, FormatPercent, FormatDateTime,
/// Str and Val. Output follows the invariant en-US conventions: "." decimal
/// point, "," thousands separator, "$" currency, m/d/yyyy dates, 12-hour
/// times with AM/PM.
module Rbt.Vb.Formatting

open System
open System.Text
open Values
open ValueSemantics
open NumberText

// ── Sections and tokens ──

/// Split a format on `;` outside quotes and escapes.
let splitSections (format: string) =
    let sections = ResizeArray<string>()
    let current = StringBuilder()
    let mutable i = 0
    let mutable quoted = false
    while i < format.Length do
        let c = format.[i]
        if quoted then
            current.Append c |> ignore
            if c = '"' then quoted <- false
        elif c = '"' then
            quoted <- true
            current.Append c |> ignore
        elif c = '\\' && i + 1 < format.Length then
            current.Append(c).Append(format.[i + 1]) |> ignore
            i <- i + 1
        elif c = ';' then
            sections.Add(current.ToString())
            current.Clear() |> ignore
        else current.Append c |> ignore
        i <- i + 1
    sections.Add(current.ToString())
    List.ofSeq sections

/// The format characters outside quotes and escapes, lowercased.
let private unquoted (section: string) =
    let sb = StringBuilder()
    let mutable i = 0
    let mutable quoted = false
    while i < section.Length do
        let c = section.[i]
        if quoted then (if c = '"' then quoted <- false)
        elif c = '"' then quoted <- true
        elif c = '\\' then i <- i + 1
        else sb.Append(Char.ToLowerInvariant c) |> ignore
        i <- i + 1
    sb.ToString()

let private hasDigitPlaceholders (section: string) =
    let text = unquoted section
    text.IndexOf '0' >= 0 || text.IndexOf '#' >= 0

let private hasStringPlaceholders (section: string) =
    let text = unquoted section
    text.IndexOf '@' >= 0 || text.IndexOf '&' >= 0 || text = "<" || text = ">" || text = "!" 

let private hasDateTokens (section: string) =
    let text = unquoted section
    text |> Seq.exists (fun c -> "dmyhnswqct".IndexOf c >= 0) || text.Contains "am/pm" || text.Contains "a/p"

// ── Numbers ──

type private NumToken =
    | Digit of char
    | Point
    | Comma
    | Percent
    | Exponent of string * int   // "E+" / "E-" / "e+" / "e-", minimum exponent digits
    | Lit of string

let private tokenizeNumber (section: string) =
    let tokens = ResizeArray<NumToken>()
    let mutable i = 0
    let mutable seenPoint = false
    while i < section.Length do
        let c = section.[i]
        match c with
        | '"' ->
            let close = section.IndexOf('"', i + 1)
            let stop = if close < 0 then section.Length else close
            tokens.Add(Lit (section.Substring(i + 1, stop - i - 1)))
            i <- stop + 1
        | '\\' when i + 1 < section.Length ->
            tokens.Add(Lit (string section.[i + 1]))
            i <- i + 2
        | '0' | '#' -> tokens.Add(Digit c); i <- i + 1
        | '.' when not seenPoint -> seenPoint <- true; tokens.Add Point; i <- i + 1
        | ',' -> tokens.Add Comma; i <- i + 1
        | '%' -> tokens.Add Percent; i <- i + 1
        | ('E' | 'e') when i + 1 < section.Length && (section.[i + 1] = '+' || section.[i + 1] = '-') ->
            let marker = section.Substring(i, 2)
            i <- i + 2
            let mutable digits = 0
            while i < section.Length && (section.[i] = '0' || section.[i] = '#') do
                if section.[i] = '0' then digits <- digits + 1
                i <- i + 1
            tokens.Add(Exponent (marker, max digits 1))
        | _ -> tokens.Add(Lit (string c)); i <- i + 1
    List.ofSeq tokens

/// Format a non-negative magnitude with one numeric section.
let private formatMagnitude (magnitude: Digits) (section: string) =
    let tokens = tokenizeNumber section
    let percents = tokens |> List.filter (fun t -> t = Percent) |> List.length
    let pointIndex = tokens |> List.tryFindIndex (fun t -> t = Point)
    let expIndex = tokens |> List.tryFindIndex (function Exponent _ -> true | _ -> false)
    let intEnd = match pointIndex, expIndex with | Some p, _ -> p | None, Some e -> e | None, None -> tokens.Length
    let intTokens = tokens |> List.take intEnd
    let fracTokens =
        match pointIndex with
        | Some p ->
            let stop = match expIndex with Some e -> e | None -> tokens.Length
            tokens |> List.skip (p + 1) |> List.take (stop - p - 1)
        | None -> []
    let isDigit t = match t with Digit _ -> true | _ -> false
    let intDigits = intTokens |> List.filter isDigit
    // A comma between digit placeholders groups thousands; commas right after
    // the last integer placeholder scale by 1000 each.
    let lastDigit = intTokens |> List.tryFindIndexBack isDigit
    let firstDigit = intTokens |> List.tryFindIndex isDigit
    let mutable grouping = false
    let mutable scale = 0
    intTokens |> List.iteri (fun i t ->
        if t = Comma then
            match firstDigit, lastDigit with
            | Some f, Some l when i > f && i < l -> grouping <- true
            | Some _, Some l when i > l -> scale <- scale + 1
            | _ -> ())
    let decimals = fracTokens |> List.filter isDigit |> List.length
    let shifted = { magnitude with Exponent = (if magnitude.Digits = "" then 0 else magnitude.Exponent + 2 * percents - 3 * scale) }
    let integerDigits, fractionDigits, exponentText =
        match expIndex with
        | Some e ->
            let marker, minDigits = match tokens.[e] with Exponent (m, d) -> m, d | _ -> "E+", 1
            let intCount = max 1 intDigits.Length
            let rounded = roundSignificant shifted (intCount + decimals)
            let exponent = if rounded.Digits = "" then 0 else rounded.Exponent - intCount
            let mantissa = { rounded with Exponent = (if rounded.Digits = "" then 0 else intCount) }
            let expDigits = string (abs exponent)
            let expDigits = String('0', max 0 (minDigits - expDigits.Length)) + expDigits
            let sign = if exponent < 0 then "-" elif marker.[1] = '+' then "+" else ""
            integerPart mantissa, fractionPart mantissa decimals, string marker.[0] + sign + expDigits
        | None ->
            let rounded = roundDecimals shifted decimals
            integerPart rounded, fractionPart rounded decimals, ""
    // Trailing zeros of '#' decimal placeholders are dropped.
    let fractionDigits =
        let placeholders = fracTokens |> List.choose (function Digit c -> Some c | _ -> None) |> List.toArray
        let mutable stop = fractionDigits.Length
        while stop > 0 && placeholders.[stop - 1] = '#' && fractionDigits.[stop - 1] = '0' do stop <- stop - 1
        fractionDigits.Substring(0, stop)
    let integerDigits =
        if integerDigits = "0" && not (intDigits |> List.contains (Digit '0')) then "" else integerDigits
    let sb = StringBuilder()
    // Integer part: digits fill the placeholders from the right; extra digits
    // go to the first placeholder. With grouping, the whole run is written there.
    let placeholderCount = intDigits.Length
    let padded =
        if integerDigits.Length >= placeholderCount then integerDigits
        else String(' ', placeholderCount - integerDigits.Length) + integerDigits
    let extra = padded.Length - placeholderCount
    let mutable position = 0
    let mutable first = true
    for t in intTokens do
        match t with
        | Digit p ->
            if grouping then
                if first then sb.Append(groupThousands (integerDigits.TrimStart ' ')) |> ignore
            else
                let count = if first then extra + 1 else 1
                for k in 0 .. count - 1 do
                    let ch = padded.[position + k]
                    if ch <> ' ' then sb.Append ch |> ignore
                    elif p = '0' then sb.Append '0' |> ignore
                position <- position + count
            first <- false
        | Comma -> ()
        | Percent -> sb.Append '%' |> ignore
        | Lit text -> sb.Append text |> ignore
        | _ -> ()
    if grouping && first then sb.Append(groupThousands integerDigits) |> ignore
    if pointIndex.IsSome then sb.Append '.' |> ignore
    let mutable fracPosition = 0
    for t in fracTokens do
        match t with
        | Digit _ ->
            if fracPosition < fractionDigits.Length then sb.Append fractionDigits.[fracPosition] |> ignore
            fracPosition <- fracPosition + 1
        | Percent -> sb.Append '%' |> ignore
        | Lit text -> sb.Append text |> ignore
        | Comma -> ()
        | _ -> ()
    match expIndex with
    | Some e ->
        sb.Append exponentText |> ignore
        for t in tokens |> List.skip (e + 1) do
            match t with
            | Lit text -> sb.Append text |> ignore
            | Percent -> sb.Append '%' |> ignore
            | _ -> ()
    | None -> ()
    sb.ToString(), (integerDigits.Trim() + fractionDigits |> Seq.forall (fun c -> c = '0'))

let private numberDigits (value: Value) =
    match value with
    | VCurrency units -> ofScaled units 4, units < 0L
    | VInteger n | VLong n -> ofText (string n), n < 0
    | VByte n -> ofText (string n), false
    | VSingle f -> ofSingleDigits f, f < 0.0f
    | other ->
        let x = toNumber other
        roundSignificant (ofFloat x) 15, x < 0.0

let private isZeroValue (value: Value) =
    match value with
    | VCurrency units -> units = 0L
    | other -> toNumber other = 0.0

/// A number with a user-defined numeric format ("#,##0.00;(#,##0.00);\"zero\"").
let formatNumberWith (value: Value) (format: string) =
    let sections = splitSections format
    let digits, negative = numberDigits value
    let zero = isZeroValue value
    match sections with
    | [ single ] ->
        let text, roundedZero = formatMagnitude digits single
        if negative && not roundedZero then "-" + text else text
    | positive :: negativeSection :: rest ->
        if zero && not rest.IsEmpty && rest.Head <> "" then fst (formatMagnitude zeroDigits rest.Head)
        elif negative then
            if negativeSection = "" then "-" + fst (formatMagnitude digits positive)
            else fst (formatMagnitude digits negativeSection)
        else fst (formatMagnitude digits positive)
    | [] -> ""

// ── Dates ──

type private DateToken =
    | DT of string    // lowercased token: d, dd, ..., m, mm, n, ..., ampm variants
    | DLit of string

let private tokenizeDate (section: string) =
    let tokens = ResizeArray<DateToken>()
    let mutable i = 0
    let lower = section.ToLowerInvariant()
    let startsAt (text: string) = i + text.Length <= section.Length && lower.Substring(i, text.Length) = text
    while i < section.Length do
        let c = lower.[i]
        if section.[i] = '"' then
            let close = section.IndexOf('"', i + 1)
            let stop = if close < 0 then section.Length else close
            tokens.Add(DLit (section.Substring(i + 1, stop - i - 1)))
            i <- stop + 1
        elif section.[i] = '\\' && i + 1 < section.Length then
            tokens.Add(DLit (string section.[i + 1]))
            i <- i + 2
        elif startsAt "am/pm" then
            tokens.Add(DT (if section.[i] = 'A' then "AM/PM" else "am/pm")); i <- i + 5
        elif startsAt "a/p" then
            tokens.Add(DT (if section.[i] = 'A' then "A/P" else "a/p")); i <- i + 3
        elif startsAt "ampm" then
            tokens.Add(DT "AMPM"); i <- i + 4
        elif "dmyhnswqct".IndexOf c >= 0 then
            let mutable j = i
            while j < section.Length && lower.[j] = c do j <- j + 1
            let run = j - i
            let add n = tokens.Add(DT (String(c, n)))
            match c with
            | 'd' -> add (min run 6)
            | 'm' -> (if run > 4 then (add 4; add (run - 4)) else add run)
            | 'y' -> (if run >= 4 then add 4 elif run = 3 then (add 2; add 1) else add run)
            | 'h' | 'n' | 's' | 'w' -> add (min run 2)
            | 't' -> (if run >= 5 then add 5 else tokens.Add(DLit (section.Substring(i, run))))
            | _ -> add 1
            i <- j
        else
            tokens.Add(DLit (string section.[i]))
            i <- i + 1
    let tokens = tokens.ToArray()
    // "m" right after an hour or right before a second is the minute.
    let isHour t = match t with DT ("h" | "hh") -> true | _ -> false
    let isSecond t = match t with DT ("s" | "ss") -> true | _ -> false
    let isDateTimeToken t = match t with DT _ -> true | _ -> false
    for k in 0 .. tokens.Length - 1 do
        match tokens.[k] with
        | DT ("m" | "mm" as m) ->
            let previous = tokens.[.. k - 1] |> Array.tryFindBack isDateTimeToken
            let next = tokens.[k + 1 ..] |> Array.tryFind isDateTimeToken
            if (previous |> Option.exists isHour) || (next |> Option.exists isSecond) then
                tokens.[k] <- DT (if m = "m" then "n" else "nn")
        | _ -> ()
    tokens

let private pad2 (n: int) = if n < 10 then "0" + string n else string n

let longDate (serial: float) =
    let y, m, d, _, _, _ = DateCore.components serial
    DateCore.dayNames.[DateCore.weekday serial 1 - 1] + ", " + DateCore.monthNames.[m - 1] + " " + string d + ", " + string y

/// A date with a user-defined date/time format ("yyyy-mm-dd hh:nn:ss").
let formatDateWith (serial: float) (section: string) (firstDay: int) (firstWeek: int) =
    let tokens = tokenizeDate section
    let twelveHour = tokens |> Array.exists (function DT ("AM/PM" | "am/pm" | "A/P" | "a/p" | "AMPM") -> true | _ -> false)
    let y, m, d, h, n, s = DateCore.components serial
    let sb = StringBuilder()
    for t in tokens do
        match t with
        | DLit text -> sb.Append text |> ignore
        | DT token ->
            let hour12 = if h % 12 = 0 then 12 else h % 12
            let text =
                match token with
                | "c" -> DateCore.generalDate serial
                | "d" -> string d
                | "dd" -> pad2 d
                | "ddd" -> DateCore.abbreviation DateCore.dayNames.[DateCore.weekday serial 1 - 1]
                | "dddd" -> DateCore.dayNames.[DateCore.weekday serial 1 - 1]
                | "ddddd" -> DateCore.shortDate serial
                | "dddddd" -> longDate serial
                | "w" -> string (DateCore.weekday serial firstDay)
                | "ww" -> string (DateCore.weekOfYear serial firstDay firstWeek)
                | "m" -> string m
                | "mm" -> pad2 m
                | "mmm" -> DateCore.abbreviation DateCore.monthNames.[m - 1]
                | "mmmm" -> DateCore.monthNames.[m - 1]
                | "q" -> string ((m - 1) / 3 + 1)
                | "y" -> string (DateCore.datePart serial - DateCore.dayNumber y 1 1 + 1)
                | "yy" -> pad2 (y % 100)
                | "yyyy" -> string y
                | "h" -> string (if twelveHour then hour12 else h)
                | "hh" -> pad2 (if twelveHour then hour12 else h)
                | "n" -> string n
                | "nn" -> pad2 n
                | "s" -> string s
                | "ss" -> pad2 s
                | "ttttt" -> DateCore.longTime serial
                | "AM/PM" | "AMPM" -> if h < 12 then "AM" else "PM"
                | "am/pm" -> if h < 12 then "am" else "pm"
                | "A/P" -> if h < 12 then "A" else "P"
                | "a/p" -> if h < 12 then "a" else "p"
                | other -> other
            sb.Append text |> ignore
    sb.ToString()

// ── Strings ──

/// A string with @ & < > ! placeholders.
let formatStringWith (text: string) (section: string) =
    let lower = section.IndexOf '<' >= 0 && not (section.Contains "\"<")
    let upper = section.IndexOf '>' >= 0 && not (section.Contains "\">")
    let leftToRight = section.IndexOf '!' >= 0
    let text = if lower then text.ToLowerInvariant() elif upper then text.ToUpperInvariant() else text
    let placeholders = section |> Seq.filter (fun c -> c = '@' || c = '&') |> Seq.length
    // Characters assigned to each placeholder, in order.
    let assigned = Array.create placeholders ""
    if placeholders > 0 then
        if leftToRight then
            for k in 0 .. placeholders - 1 do
                if k < text.Length then assigned.[k] <- string text.[k]
            if text.Length > placeholders then assigned.[placeholders - 1] <- assigned.[placeholders - 1] + text.Substring placeholders
        else
            let offset = placeholders - text.Length
            for k in 0 .. placeholders - 1 do
                let index = k - offset
                if index >= 0 then assigned.[k] <- string text.[index]
            if offset < 0 then assigned.[0] <- text.Substring(0, 1 - offset)
    let sb = StringBuilder()
    if placeholders = 0 then sb.Append text |> ignore
    let mutable k = 0
    let mutable i = 0
    while i < section.Length do
        let c = section.[i]
        match c with
        | '@' | '&' ->
            let value = assigned.[k]
            sb.Append(if value = "" && c = '@' then " " else value) |> ignore
            k <- k + 1
        | '<' | '>' | '!' -> ()
        | '"' ->
            let close = section.IndexOf('"', i + 1)
            let stop = if close < 0 then section.Length else close
            sb.Append(section.Substring(i + 1, stop - i - 1)) |> ignore
            i <- stop
        | '\\' when i + 1 < section.Length ->
            sb.Append section.[i + 1] |> ignore
            i <- i + 1
        | other -> sb.Append other |> ignore
        i <- i + 1
    sb.ToString()

// ── Format ──

let private namedDateFormat (name: string) =
    match name with
    | "long date" -> Some "dddddd"
    | "medium date" -> Some "dd-mmm-yy"
    | "short date" -> Some "m/d/yyyy"
    | "long time" -> Some "h:nn:ss AM/PM"
    | "medium time" -> Some "hh:nn AM/PM"
    | "short time" -> Some "hh:nn"
    | _ -> None

let private namedNumberFormat (name: string) =
    match name with
    | "currency" -> Some "$#,##0.00;($#,##0.00)"
    | "fixed" -> Some "0.00"
    | "standard" -> Some "#,##0.00"
    | "percent" -> Some "0.00%"
    | "scientific" -> Some "0.00E+00"
    | _ -> None

let private isNumeric (value: Value) =
    match value with
    | VInteger _ | VLong _ | VSingle _ | VByte _ | VDouble _ | VCurrency _ | VBoolean _ -> true
    | VString text -> (tryParseNumber text).IsSome
    | _ -> false

/// Format(expression, format, firstDayOfWeek, firstWeekOfYear).
let format (value: Value) (format: string) (firstDay: int) (firstWeek: int) : Value =
    let value = scalar value
    let name = format.Trim().ToLowerInvariant()
    let asNumber v = match v with VBoolean b -> VInteger (if b then -1 else 0) | VString t -> VDouble (toNumber (VString t)) | other -> other
    match value with
    | VNull ->
        match splitSections format with
        | [ _; _; _; nullSection ] -> VString (fst (formatMagnitude zeroDigits nullSection))
        | _ -> VNull
    | VEmpty when format = "" -> VString ""
    | _ when format = "" ->
        match value with
        | VBoolean b -> VString (if b then "True" else "False")
        | other -> VString (toString other)
    | _ ->
    match name with
    | "general number" ->
        if isNumeric value then
            let precision = match value with VSingle _ -> 7 | _ -> 15
            VString (let x = toNumber (asNumber value) in (if x < 0.0 then "-" else "") + general (ofFloat x) precision)
        else VString (toString value)
    | "yes/no" | "true/false" | "on/off" ->
        let truthy = if isNumeric value then toNumber (asNumber value) <> 0.0 else toBool value
        let yes, no = match name with "yes/no" -> "Yes", "No" | "true/false" -> "True", "False" | _ -> "On", "Off"
        VString (if truthy then yes else no)
    | "general date" ->
        match value with
        | VDate serial -> VString (DateCore.generalDate serial)
        | _ when isNumeric value -> VString (DateCore.generalDate (toNumber (asNumber value)))
        | VString text -> (match tryParseDate text with Some serial -> VString (DateCore.generalDate serial) | None -> VString text)
        | other -> VString (toString other)
    | _ ->
    match namedNumberFormat name, namedDateFormat name with
    | Some numeric, _ when isNumeric value -> VString (formatNumberWith (asNumber value) numeric)
    | Some _, _ -> VString (toString value)
    | None, Some dateFormat ->
        match value with
        | VDate serial -> VString (formatDateWith serial dateFormat firstDay firstWeek)
        | VString text ->
            match tryParseDate text with
            | Some serial -> VString (formatDateWith serial dateFormat firstDay firstWeek)
            | None -> VString text
        | _ when isNumeric value -> VString (formatDateWith (toNumber (asNumber value)) dateFormat firstDay firstWeek)
        | other -> VString (toString other)
    | None, None ->
        let first = (splitSections format).Head
        match value with
        | VDate serial when hasDateTokens first && not (hasDigitPlaceholders first) ->
            VString (formatDateWith serial first firstDay firstWeek)
        | VDate serial when hasDigitPlaceholders first -> VString (formatNumberWith (VDouble serial) format)
        | _ when hasStringPlaceholders first && not (hasDigitPlaceholders first) ->
            VString (formatStringWith (toString value) first)
        | _ when isNumeric value && hasDigitPlaceholders first -> VString (formatNumberWith (asNumber value) format)
        | _ when isNumeric value && hasDateTokens first -> VString (formatDateWith (toNumber (asNumber value)) first firstDay firstWeek)
        | VString text when hasDateTokens first ->
            match tryParseDate text with
            | Some serial -> VString (formatDateWith serial first firstDay firstWeek)
            | None -> VString text
        | _ when isNumeric value -> VString (formatNumberWith (asNumber value) format)
        | other -> VString (toString other)

// ── FormatNumber / FormatCurrency / FormatPercent / FormatDateTime ──

/// vbTrue (-1) / vbFalse (0) / vbUseDefault (-2) options.
let private triState (value: int) (fallback: bool) =
    match value with
    | -2 -> fallback
    | 0 -> false
    | _ -> true

let formatFixed (number: Value) (decimals: int) (leadingDigit: int) (parens: int) (group: int) (parensDefault: bool) (prefix: string) (suffix: string) =
    let digits, negative = numberDigits number
    let decimals = if decimals < 0 then 2 else decimals
    let rounded = roundDecimals digits decimals
    let integer = integerPart rounded
    let integer = if triState group true then groupThousands integer else integer
    let integer = if integer = "0" && not (triState leadingDigit true) && decimals > 0 then "" else integer
    let fraction = fractionPart rounded decimals
    let body = prefix + integer + (if decimals > 0 then "." + fraction else "") + suffix
    let isZero = rounded.Digits = ""
    if negative && not isZero then
        if triState parens parensDefault then "(" + body + ")" else "-" + body
    else body

let formatDateTime (serial: float) (namedFormat: int) =
    match namedFormat with
    | 0 -> DateCore.generalDate serial
    | 1 -> longDate serial
    | 2 -> DateCore.shortDate serial
    | 3 -> DateCore.longTime serial
    | 4 -> formatDateWith serial "hh:nn" 1 1
    | _ -> raiseVbError 5

// ── Str and Val ──

/// Str: a leading space for non-negative numbers.
let rec str (value: Value) =
    match scalar value with
    | VNull -> VNull
    | VString text -> str (VDouble (toNumber (VString text)))
    | (VDate _ | VBoolean _ | VError _) as other -> VString (toString other)
    | other ->
        // Str writes no 0 before the decimal point: Str(0.5) is " .5".
        let text = toString other
        let text =
            if text.StartsWith "0." then text.Substring 1
            elif text.StartsWith "-0." then "-" + text.Substring 2
            else text
        VString (if text.StartsWith "-" then text else " " + text)

/// Val: the number at the start of a string, ignoring spaces, tabs and line
/// feeds; "&H" and "&O" prefixes read hexadecimal and octal.
let valOf (text: string) =
    let sb = StringBuilder()
    for c in text do
        if c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r' then sb.Append c |> ignore
    let t = sb.ToString()
    if t.Length >= 2 && t.[0] = '&' && (Char.ToLowerInvariant t.[1] = 'h' || Char.ToLowerInvariant t.[1] = 'o' || Char.IsDigit t.[1]) then
        // "&H" is hexadecimal; "&O" and a bare "&" are octal.
        let radix = if Char.ToLowerInvariant t.[1] = 'h' then 16 else 8
        let mutable i = if Char.IsDigit t.[1] then 1 else 2
        let mutable value = 0.0
        let digitOf (c: char) =
            let c = Char.ToLowerInvariant c
            if c >= '0' && c <= '9' then int c - int '0'
            elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
            else 99
        while i < t.Length && digitOf t.[i] < radix do
            value <- value * float radix + float (digitOf t.[i])
            i <- i + 1
        let isLong = i < t.Length && t.[i] = '&'
        if not isLong && value <= 65535.0 && value >= 32768.0 then value - 65536.0
        elif value >= 2147483648.0 && value <= 4294967295.0 then value - 4294967296.0
        else value
    else
        let mutable i = 0
        let n = t.Length
        if i < n && (t.[i] = '+' || t.[i] = '-') then i <- i + 1
        let digitsStart = i
        while i < n && Char.IsDigit t.[i] do i <- i + 1
        if i < n && t.[i] = '.' then
            i <- i + 1
            while i < n && Char.IsDigit t.[i] do i <- i + 1
        let mantissaEnd = i
        let hasDigits = mantissaEnd > digitsStart && not (mantissaEnd = digitsStart + 1 && t.[digitsStart] = '.')
        if not hasDigits then 0.0
        else
            let mutable stop = mantissaEnd
            if i < n && (t.[i] = 'e' || t.[i] = 'E' || t.[i] = 'd' || t.[i] = 'D') then
                let mutable j = i + 1
                if j < n && (t.[j] = '+' || t.[j] = '-') then j <- j + 1
                let expStart = j
                while j < n && Char.IsDigit t.[j] do j <- j + 1
                if j > expStart then stop <- j
            let number = t.Substring(0, stop).Replace('d', 'e').Replace('D', 'e')
#if FABLE_COMPILER
            match Double.TryParse number with
#else
            match Double.TryParse(number, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
#endif
            | true, value when Double.IsInfinity value -> raiseVbError 6
            | true, value -> value
            | _ -> 0.0
