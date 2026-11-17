module Rbt.Vb.ValueSemantics

open System
open Values

let rec dereference = function
    | VRef (values, index) -> dereference values.[index]
    | VFieldRef (instance, name) -> dereference instance.Fields.[name]
    | value -> value

let private typeMismatch () = raiseVbError 13
let private nullError () = raiseVbError 94

/// The default property of a host object (Let-coercion): `GetMember ""`, then
/// `"_Default"`, then `"Value"`. An object without one raises error 438.
let hostDefault (host: IHostObject) =
    match host.GetMember "" with
    | Some value -> value
    | None ->
        match host.GetMember "_Default" with
        | Some value -> value
        | None ->
            match host.GetMember "Value" with
            | Some value -> value
            | None -> raiseVbError 438

/// Reads the default member of a VB class instance (installed by the VM,
/// which runs the class's Property Get). `None` when the class has none.
let mutable objectDefault : VBObject -> Value option = fun _ -> None

/// Dereference and, for objects, read through the default property
/// (repeatedly, for a default that is itself an object).
let rec scalar value =
    match dereference value with
    | VHostObject host ->
        match hostDefault host with
        | VHostObject next when obj.ReferenceEquals(next, host) -> raiseVbError 438
        | next -> scalar next
    | VObject o as original ->
        match objectDefault o with
        | Some (VObject next) when obj.ReferenceEquals(next.Fields, o.Fields) -> raiseVbError 438
        | Some next -> scalar next
        | None -> original
    | other -> other

/// A number written as text: "12", " 1.5e3 ", "1,234.5", "&HFF", "&O17".
let tryParseNumber (text: string) =
    let t = if isNull text then "" else text.Trim()
    if t.Length > 2 && t.[0] = '&' && (t.[1] = 'h' || t.[1] = 'H' || t.[1] = 'o' || t.[1] = 'O') then
        let radix = if t.[1] = 'h' || t.[1] = 'H' then 16 else 8
        let body = t.Substring(2).TrimEnd('&')
        let mutable value = 0.0
        let mutable ok = body.Length > 0
        for c in body do
            let digit =
                if c >= '0' && c <= '9' then int c - int '0'
                elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
                elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
                else 99
            if digit >= radix then ok <- false
            value <- value * float radix + float digit
        if not ok || value > 4294967295.0 then None
        elif value <= 65535.0 && not (t.EndsWith "&") then Some (if value >= 32768.0 then value - 65536.0 else value)
        elif value >= 2147483648.0 then Some (value - 4294967296.0)
        else Some value
    else
        let t = t.Replace(",", "")
        let isNumberChar c = (c >= '0' && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-'
        if t = "" || not (Seq.forall isNumberChar t) || not (Seq.exists Char.IsDigit t) then None
        else
#if FABLE_COMPILER
            match Double.TryParse t with
#else
            match Double.TryParse(t, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
#endif
            | true, number when not (Double.IsNaN number || Double.IsInfinity number) -> Some number
            | _ -> None

let private parseNumber (text: string) =
    match tryParseNumber text with
    | Some number -> number
    | None -> typeMismatch ()

/// The current year, for dates written without one ("1/31").
let currentYear () = DateTime.Now.Year

/// A date read from text (CDate), or None.
let tryParseDate (text: string) =
    match DateCore.tryParse text (currentYear ()) with
    | Some serial -> Some serial
    | None -> None

let currencyScale = 10000.0

let toNumber value =
    match value with
    | VInteger number -> float number
    | VDouble number -> number
    | _ ->
    match scalar value with
    | VInteger number -> float number
    | VDouble number -> number
    | VBoolean value -> if value then -1.0 else 0.0
    | VEmpty -> 0.0
    | VString text -> parseNumber text
    | VDate serial -> serial
    | VCurrency units -> float units / currencyScale
    | VNull -> nullError ()
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> typeMismatch ()

let private checkedInt (minimum: float) (maximum: float) (number: float) =
    if Double.IsNaN number || Double.IsInfinity number || number < minimum || number > maximum then
        raiseVbError 6
    int number

let toInteger value =
    match value with
    | VInteger number -> number
    | _ ->
        let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
        checkedInt (float Int32.MinValue) (float Int32.MaxValue) rounded

/// The 1/10000 units of a value converted to Currency (rounding to even).
let rec toCurrencyUnits value =
    match scalar value with
    | VCurrency units -> units
    | VInteger n -> int64 n * 10000L
    | VString text -> toCurrencyUnits (VDouble (parseNumber text))
    | other ->
        let number = toNumber other
        let digits = NumberText.roundSignificant (NumberText.ofFloat number) 15
        // Scale through the decimal digits (0.1 * 10000 is exactly 1000).
        let scaled =
            if digits.Digits = "" then 0.0
            else
                let units = NumberText.integerPart { digits with Exponent = digits.Exponent + 4 }
                let fraction = NumberText.fractionPart { digits with Exponent = digits.Exponent + 4 } 1
                let whole = float units
                let rounded =
                    if fraction.[0] > '5' then whole + 1.0
                    elif fraction.[0] < '5' then whole
                    else
                        let rest = NumberText.significantFraction { digits with Exponent = digits.Exponent + 4 }
                        if rest.Length > 1 then whole + 1.0
                        elif whole % 2.0 <> 0.0 then whole + 1.0
                        else whole
                if number < 0.0 then -rounded else rounded
        if Math.Abs scaled > 9.2233720368547758E18 then raiseVbError 6
        int64 scaled

let rec toString value =
    match value with
    | VString text -> text
    | _ ->
    match scalar value with
    | VInteger number -> string number
    | VDouble number -> NumberText.ofDouble number
    | VString text -> text
    | VBoolean value -> if value then "True" else "False"
    | VDate serial -> DateCore.generalDate serial
    | VCurrency units -> NumberText.ofCurrency units
    | VError number -> "Error " + string number
    | VEmpty -> ""
    | VNull -> nullError ()
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> typeMismatch ()

let toBool value =
    match scalar value with
    | VBoolean boolean -> boolean
    | VEmpty -> false
    | VNull -> nullError ()
    | VString text when String.Equals(text.Trim(), "True", StringComparison.OrdinalIgnoreCase) -> true
    | VString text when String.Equals(text.Trim(), "False", StringComparison.OrdinalIgnoreCase) -> false
    | VCurrency units -> units <> 0L
    | value -> toNumber value <> 0.0

/// CDate: a Date from a number, a date string or a Date.
let toDate value =
    match scalar value with
    | VDate serial -> serial
    | VString text ->
        match tryParseDate text with
        | Some serial -> serial
        | None ->
            match tryParseNumber text with
            | Some number when DateCore.isValidSerial number -> number
            | Some _ -> raiseVbError 6
            | None -> typeMismatch ()
    | VNull -> nullError ()
    | other ->
        let serial = toNumber other
        if not (DateCore.isValidSerial serial) then raiseVbError 6
        serial

let inline private fitsInt (number: float) = number >= -2147483648.0 && number <= 2147483647.0

/// Operand for arithmetic: Empty acts as the Integer 0.
let private arithmetic value =
    match scalar value with
    | VEmpty -> VInteger 0
    | other -> other

let private checkedDate (serial: float) =
    if not (DateCore.isValidSerial serial) then raiseVbError 6
    VDate serial

let private checkedCurrency (units: float) =
    if Double.IsNaN units || Math.Abs units > 9.2233720368547758E18 then raiseVbError 6
    VCurrency (int64 units)

let private currencySum (l: int64) (r: int64) negate =
    let r = if negate then -r else r
    let sum = l + r
    // Overflow when both operands have the same sign and the sum does not.
    if (l >= 0L) = (r >= 0L) && (sum >= 0L) <> (l >= 0L) then raiseVbError 6
    VCurrency sum

/// Date and Currency operands of + and -: Date wins, then Currency (VB's
/// order of precision puts Currency above Double).
let private mixedAddSubtract (l: Value) (r: Value) (subtract: bool) =
    match l, r with
    | VDate a, VDate b when subtract -> VDouble (a - b)
    | VDate _, _ | _, VDate _ ->
        let a = toNumber l
        let b = toNumber r
        checkedDate (if subtract then a - b else a + b)
    | VCurrency a, VCurrency b -> currencySum a b subtract
    | VCurrency _, _ | _, VCurrency _ -> currencySum (toCurrencyUnits l) (toCurrencyUnits r) subtract
    | _ -> VDouble (if subtract then toNumber l - toNumber r else toNumber l + toNumber r)

/// Integer results that leave the 32-bit range are promoted to Double, as VB
/// does for Variants; typed Integer/Long variables raise Overflow on store.
let addValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l + float r
        if fitsInt f then VInteger (l + r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l + r)
    | VDouble l, VInteger r -> VDouble (l + float r)
    | VInteger l, VDouble r -> VDouble (float l + r)
    | _ ->
    match scalar left, scalar right with
    | VString l, VString r -> VString (l + r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l + float r
        if fitsInt f then VInteger (l + r) else VDouble f
    | (VDate _ | VCurrency _ as l), r | l, (VDate _ | VCurrency _ as r) -> mixedAddSubtract l r false
    | l, r -> VDouble (toNumber l + toNumber r)

let subtractValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l - float r
        if fitsInt f then VInteger (l - r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l - r)
    | VDouble l, VInteger r -> VDouble (l - float r)
    | VInteger l, VDouble r -> VDouble (float l - r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l - float r
        if fitsInt f then VInteger (l - r) else VDouble f
    | (VDate _ | VCurrency _ as l), r | l, (VDate _ | VCurrency _ as r) -> mixedAddSubtract l r true
    | l, r -> VDouble (toNumber l - toNumber r)

let private currencyProduct (a: int64) (b: int64) =
    let product = (decimal a * decimal b) / 10000M
    let rounded = Math.Round(product, 0, MidpointRounding.ToEven)
    if rounded > 9223372036854775807M || rounded < -9223372036854775808M then raiseVbError 6
    VCurrency (int64 rounded)

let multiplyValues left right =
    match left, right with
    | VInteger l, VInteger r ->
        let f = float l * float r
        if fitsInt f then VInteger (l * r) else VDouble f
    | VDouble l, VDouble r -> VDouble (l * r)
    | VDouble l, VInteger r -> VDouble (l * float r)
    | VInteger l, VDouble r -> VDouble (float l * r)
    | _ ->
    match arithmetic left, arithmetic right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r ->
        let f = float l * float r
        if fitsInt f then VInteger (l * r) else VDouble f
    | VCurrency a, VInteger n | VInteger n, VCurrency a -> checkedCurrency (float a * float n) |> ignore; VCurrency (a * int64 n)
    | (VCurrency _ as l), r | l, (VCurrency _ as r) -> currencyProduct (toCurrencyUnits l) (toCurrencyUnits r)
    | l, r -> VDouble (toNumber l * toNumber r)

let negateValue value =
    match scalar value with
    | VInteger i when i <> Int32.MinValue -> VInteger (-i)
    | VInteger i -> VDouble (-(float i))
    | VDouble d -> VDouble (-d)
    | VDate d -> checkedDate (-d)
    | VCurrency c -> VCurrency (-c)
    | VNull -> VNull
    | VEmpty -> VInteger 0
    | other -> VDouble (-(toNumber other))

// VB arithmetic treats Empty as zero, True as -1, and propagates Null.
// Preserve an integral result only when both operands really are integers;
// coercing a Double or numeric string must not truncate it to an integer.
let numericBinOp (integerOp: int -> int -> int) (doubleOp: float -> float -> float) left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r -> VInteger (integerOp l r)
    | l, r -> VDouble (doubleOp (toNumber l) (toNumber r))

// Binary comparison is the VBScript / VBA default (Option Compare Binary).
// Explicit text comparison is case-insensitive; use one policy for builtins
// and the VM rather than implementing a different ordering in each.
let compareStrings textMode (left: string) (right: string) =
    if textMode then String.Compare(left, right, StringComparison.OrdinalIgnoreCase)
    else String.Compare(left, right, StringComparison.Ordinal)

/// A Date compared with a string reads the string as a date when it is one.
let private dateOperand (value: Value) =
    match value with
    | VString text ->
        match tryParseDate text with
        | Some serial -> serial
        | None -> toNumber value
    | other -> toNumber other

let compareValues textMode left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VString l, VString r -> VInteger (sign (compareStrings textMode l r))
    | VString l, VEmpty -> VInteger (sign (compareStrings textMode l ""))
    | VEmpty, VString r -> VInteger (sign (compareStrings textMode "" r))
    | VCurrency l, VCurrency r -> VInteger (compare l r)
    | (VDate _ as l), r | l, (VDate _ as r) -> VInteger (compare (dateOperand l) (dateOperand r))
    | VError _, _ | _, VError _ -> typeMismatch ()
    | l, r -> VInteger (compare (toNumber l) (toNumber r))

let numericCmp (integerOp: int -> int -> bool) (doubleOp: float -> float -> bool) left right =
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | VInteger l, VInteger r -> VBoolean (integerOp l r)
    | l, r -> VBoolean (doubleOp (toNumber l) (toNumber r))

let roundedInteger minimum maximum value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    checkedInt minimum maximum rounded

let floorValue value =
    match scalar value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | VDate d -> VDate (Math.Floor d)
    | VCurrency c -> VCurrency (int64 (Math.Floor(float c / currencyScale)) * 10000L)
    | VEmpty -> VInteger 0
    | other -> VDouble (Math.Floor(toNumber other))

let truncateValue value =
    match scalar value with
    | VNull -> VNull
    | VInteger _ as integer -> integer
    | VDate d -> VDate (Math.Truncate d)
    | VCurrency c -> VCurrency ((c / 10000L) * 10000L)
    | VEmpty -> VInteger 0
    | other -> VDouble (Math.Truncate(toNumber other))

// ── Declared types ──

/// Type codes for `Dim x As <type>` coercion (see Bytecode.Coerce).
[<RequireQualifiedAccess>]
type DeclaredType =
    | Integer = 1
    | Long = 2
    | Single = 3
    | Double = 4
    | Currency = 5
    | String = 6
    | Boolean = 7
    | Byte = 8
    | Object = 9
    | Date = 10
    | LongLong = 11

let declaredTypeOf (name: string) =
    match name.ToLowerInvariant() with
    | "integer" -> Some DeclaredType.Integer
    | "long" | "longptr" -> Some DeclaredType.Long
    | "longlong" -> Some DeclaredType.LongLong
    | "single" -> Some DeclaredType.Single
    | "double" | "decimal" -> Some DeclaredType.Double
    | "currency" -> Some DeclaredType.Currency
    | "string" -> Some DeclaredType.String
    | "boolean" -> Some DeclaredType.Boolean
    | "byte" -> Some DeclaredType.Byte
    | "object" -> Some DeclaredType.Object
    | "date" -> Some DeclaredType.Date
    | _ -> None

let defaultOf (t: DeclaredType) =
    match t with
    | DeclaredType.Integer | DeclaredType.Long | DeclaredType.Byte | DeclaredType.LongLong -> VInteger 0
    | DeclaredType.Single | DeclaredType.Double -> VDouble 0.0
    | DeclaredType.Currency -> VCurrency 0L
    | DeclaredType.Date -> VDate 0.0
    | DeclaredType.String -> VString ""
    | DeclaredType.Boolean -> VBoolean false
    | DeclaredType.Object -> VNothing
    | _ -> VEmpty

/// CLngLng: a whole number, kept as Long when it fits and as Double beyond.
let toLongLong value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    if Double.IsNaN rounded || Math.Abs rounded > 9.2233720368547758E18 then raiseVbError 6
    if fitsInt rounded then VInteger (int rounded) else VDouble rounded

/// Convert a value assigned to a variable of a declared type.
let coerceTo (t: DeclaredType) (value: Value) =
    match t, value with
    | DeclaredType.Long, VInteger _ -> value
    | DeclaredType.Double, VDouble _ -> value
    | DeclaredType.String, VString _ -> value
    | DeclaredType.Boolean, VBoolean _ -> value
    | DeclaredType.Date, VDate _ -> value
    | DeclaredType.Currency, VCurrency _ -> value
    | DeclaredType.Object, (VObject _ | VHostObject _ | VNothing) -> value
    | DeclaredType.Object, _ -> raiseVbError 13
    | _ ->
    match t, scalar value with
    | _, VNull -> raiseVbError 94
    | DeclaredType.Integer, v -> VInteger (roundedInteger -32768.0 32767.0 v)
    | DeclaredType.Long, v -> VInteger (roundedInteger -2147483648.0 2147483647.0 v)
    | DeclaredType.LongLong, v -> toLongLong v
    | DeclaredType.Byte, v -> VInteger (roundedInteger 0.0 255.0 v)
    | DeclaredType.Single, v -> VDouble (float (float32 (toNumber v)))
    | DeclaredType.Double, v -> VDouble (toNumber v)
    | DeclaredType.Currency, v -> VCurrency (toCurrencyUnits v)
    | DeclaredType.Date, v -> VDate (toDate v)
    | DeclaredType.String, v -> VString (toString v)
    | DeclaredType.Boolean, v -> VBoolean (toBool v)
    | _, v -> v
