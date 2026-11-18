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
    let t = if isNull text then "" else text.Trim([| ' '; '\t'; '\r'; '\n'; '\u00A0' |])
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
        // VB also reads "$5", "1,234", "(5)", "5-" and "1D2".
        let mutable body = t
        let mutable negative = false
        let mutable signed = false
        if body.Length >= 2 && body.[0] = '(' && body.[body.Length - 1] = ')' then
            negative <- true
            signed <- true
            body <- body.Substring(1, body.Length - 2).Trim()
        let mutable scanning = true
        let mutable dollar = false
        while scanning && body.Length > 0 do
            match body.[0] with
            | ('+' | '-') as c when not signed ->
                signed <- true
                negative <- (c = '-')
                body <- body.Substring(1).TrimStart()
            | '$' when not dollar ->
                dollar <- true
                body <- body.Substring(1).TrimStart()
            | _ -> scanning <- false
        if not signed && body.Length > 1 && (body.[body.Length - 1] = '-' || body.[body.Length - 1] = '+') then
            negative <- body.[body.Length - 1] = '-'
            body <- body.Substring(0, body.Length - 1).TrimEnd()
        if not dollar && body.Length > 1 && body.[body.Length - 1] = '$' then
            body <- body.Substring(0, body.Length - 1).TrimEnd()
        let body = body.Replace('d', 'e').Replace('D', 'e')
        let isNumberChar c = (c >= '0' && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-'
        if body = "" || body.[0] = ',' || not (Seq.forall isNumberChar (body.Replace(",", ""))) then None
        else
        let body = body.Replace(",", "")
        if body = "" || not (Char.IsDigit body.[0] || body.[0] = '.') || not (Seq.exists Char.IsDigit body) then None
        else
#if FABLE_COMPILER
            match Double.TryParse body with
#else
            match Double.TryParse(body, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
#endif
            | true, number when not (Double.IsNaN number || Double.IsInfinity number) -> Some (if negative then -number else number)
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
    | VLong number -> float number
    | VDouble number -> number
    | _ ->
    match scalar value with
    | VInteger number -> float number
    | VLong number -> float number
    | VDouble number -> number
    | VSingle number -> float number
    | VByte number -> float number
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
    | VLong number -> number
    | VByte number -> int number
    | _ ->
        let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
        checkedInt (float Int32.MinValue) (float Int32.MaxValue) rounded

/// The 1/10000 units of a value converted to Currency (rounding to even).
let rec toCurrencyUnits value =
    match scalar value with
    | VCurrency units -> units
    | VInteger n | VLong n -> int64 n * 10000L
    | VByte n -> int64 n * 10000L
    | VSingle f -> toCurrencyUnits (VDouble (NumberText.singleAsDouble f))
    | other ->
        let number, digits =
            match other with
            | VString text ->
                let number = parseNumber text
                let t = text.Trim().Replace(",", "")
                // Plain decimal text keeps all its digits (CCur("922337203685477.5807")).
                if t <> "" && t |> Seq.forall (fun c -> System.Char.IsDigit c || c = '.' || c = '-' || c = '+') then
                    number, NumberText.ofText t
                else number, NumberText.roundSignificant (NumberText.ofFloat number) 15
            | _ ->
                let number = toNumber other
                number, NumberText.roundSignificant (NumberText.ofFloat number) 15
        // Scale through the decimal digits (0.1 * 10000 is exactly 1000).
        if digits.Digits = "" then 0L
        else
            let scaledDigits = { digits with Exponent = digits.Exponent + 4 }
            let units = NumberText.integerPart scaledDigits
            if units.Length > 19 then raiseVbError 6
            let whole = System.Decimal.Parse units
            let fraction = NumberText.fractionPart scaledDigits 1
            let roundUp =
                if fraction.[0] > '5' then true
                elif fraction.[0] < '5' then false
                else
                    let rest = NumberText.significantFraction scaledDigits
                    rest.Length > 1 || System.Decimal.Remainder(whole, 2M) <> 0M
            let rounded = if roundUp then whole + 1M else whole
            let signed = if number < 0.0 then -rounded else rounded
            if signed > 9223372036854775807M || signed < -9223372036854775808M then raiseVbError 6
            int64 signed

let rec toString value =
    match value with
    | VString text -> text
    | _ ->
    match scalar value with
    | VInteger number | VLong number -> string number
    | VDouble number -> NumberText.ofDouble number
    | VSingle number -> NumberText.ofSingle number
    | VByte number -> string number
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
let inline private fits16 (n: int64) = n >= -32768L && n <= 32767L
let inline private fits32 (n: int64) = n >= -2147483648L && n <= 2147483647L

/// The largest finite Single.
let maxSingle = 3.4028234663852886E38

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

let private currencyProduct (a: int64) (b: int64) =
    let product = (decimal a * decimal b) / 10000M
    let rounded = Math.Round(product, 0, MidpointRounding.ToEven)
    if rounded > 9223372036854775807M || rounded < -9223372036854775808M then raiseVbError 6
    VCurrency (int64 rounded)

// ── Numeric types of results ──
//
// VB gives an arithmetic result the type of its most precise operand, in the
// order Byte < Integer < Long < Single < Double < Currency (Boolean counts as
// Integer, a numeric String as Double, Empty takes the other operand's type).
// When the result does not fit its type, the operation raises Overflow (6) if
// both operands have declared types (`Dim i As Integer`, literals, CInt(...)),
// and Variant arithmetic widens instead: Byte -> Integer -> Long -> Double,
// Single -> Double. The compiler picks the strict or the Variant form.

[<Literal>]
let private RByte = 0
[<Literal>]
let private RInteger = 1
[<Literal>]
let private RLong = 2
[<Literal>]
let private RSingle = 3
[<Literal>]
let private RDouble = 4
[<Literal>]
let private RCurrency = 5
[<Literal>]
let private RDate = 6
[<Literal>]
let private REmpty = -1

/// The type rank of a (scalar) arithmetic operand.
let private rankOf (value: Value) =
    match value with
    | VByte _ -> RByte
    | VInteger n -> if n >= -32768 && n <= 32767 then RInteger else RLong
    | VBoolean _ -> RInteger
    | VEmpty -> REmpty
    | VLong _ -> RLong
    | VSingle _ -> RSingle
    | VDouble _ | VString _ -> RDouble
    | VCurrency _ -> RCurrency
    | VDate _ -> RDate
    | VNull -> nullError ()
    | VObject _ -> raiseVbError 438
    | VNothing -> raiseVbError 91
    | _ -> typeMismatch ()

/// The whole-number value of a Byte, Integer, Long, Boolean or Empty operand.
let private wholeOf (value: Value) =
    match value with
    | VByte b -> int64 b
    | VInteger n | VLong n -> int64 n
    | VBoolean b -> if b then -1L else 0L
    | VEmpty -> 0L
    | other -> int64 (toInteger other)

let private fitsRank rank (n: int64) =
    match rank with
    | RByte -> n >= 0L && n <= 255L
    | RInteger -> fits16 n
    | _ -> fits32 n

let private wholeOfRank rank (n: int64) =
    match rank with
    | RByte -> VByte (byte n)
    | RInteger -> VInteger (int n)
    | _ -> VLong (int n)

/// A whole-number result of type `rank` (Byte, Integer or Long).
let private wholeResult strict rank (n: int64) =
    if fitsRank rank n then wholeOfRank rank n
    elif strict then raiseVbError 6
    elif rank < RInteger && fits16 n then VInteger (int n)
    elif fits32 n then VLong (int n)
    else VDouble (float n)

let private doubleResult (x: float) =
    if Double.IsInfinity x || Double.IsNaN x then raiseVbError 6
    VDouble x

let private singleResult strict (x: float) =
    if Double.IsNaN x then raiseVbError 6
    let s = toSingle x
    if Single.IsInfinity s then
        if strict then raiseVbError 6 else doubleResult x
    else VSingle s

/// The result rank of + - * for two operand ranks.
let private resultRank lr rr =
    let lr = if lr = REmpty then (if rr = REmpty then RInteger else rr) else lr
    let rr = if rr = REmpty then lr else rr
    if (lr = RSingle && rr = RLong) || (lr = RLong && rr = RSingle) then RDouble
    else max lr rr

type private ArithmeticOp = OpAdd | OpSubtract | OpMultiply

/// Date and Currency operands of + and -: Date wins, then Currency.
let private mixedAddSubtract (l: Value) (r: Value) (subtract: bool) =
    match l, r with
    | VDate a, VDate b when subtract -> VDouble (a - b)
    | VDate _, _ | _, VDate _ ->
        let a = toNumber l
        let b = toNumber r
        checkedDate (if subtract then a - b else a + b)
    | VCurrency a, VCurrency b -> currencySum a b subtract
    | _ -> currencySum (toCurrencyUnits l) (toCurrencyUnits r) subtract

let private arithmeticOf strict (op: ArithmeticOp) (l: Value) (r: Value) =
    let lr = rankOf l
    let rr = rankOf r
    if lr = RDate || rr = RDate then
        match op with
        | OpAdd -> mixedAddSubtract l r false
        | OpSubtract -> mixedAddSubtract l r true
        | OpMultiply -> doubleResult (toNumber l * toNumber r)
    else
    let rank = resultRank lr rr
    // Currency times a Single, Double or String is a Double.
    let rank =
        if op = OpMultiply && rank = RCurrency && (lr = RSingle || rr = RSingle || lr = RDouble || rr = RDouble) then RDouble
        else rank
    if rank <= RLong then
        let a = wholeOf l
        let b = wholeOf r
        wholeResult strict rank (match op with OpAdd -> a + b | OpSubtract -> a - b | OpMultiply -> a * b)
    else
    match rank with
    | RSingle | RDouble ->
        let a = toNumber l
        let b = toNumber r
        let x = match op with OpAdd -> a + b | OpSubtract -> a - b | OpMultiply -> a * b
        if rank = RSingle then singleResult strict x else doubleResult x
    | _ ->
        match op with
        | OpAdd -> mixedAddSubtract l r false
        | OpSubtract -> mixedAddSubtract l r true
        | OpMultiply ->
            match l, r with
            | VCurrency a, (VInteger n | VLong n) | (VInteger n | VLong n), VCurrency a ->
                checkedCurrency (float a * float n) |> ignore
                VCurrency (a * int64 n)
            | _ -> currencyProduct (toCurrencyUnits l) (toCurrencyUnits r)

let private integerSum strict (left: Value) (right: Value) (l: int) (r: int) =
    let n = int64 l + int64 r
    if fits16 n && l >= -32768 && l <= 32767 && r >= -32768 && r <= 32767 then VInteger (int n)
    else wholeResult strict (max (rankOf left) (rankOf right)) n

let private longResult strict (n: int64) =
    if fits32 n then VLong (int n)
    elif strict then raiseVbError 6
    else VDouble (float n)

let private addSlow strict left right =
    match scalar left, scalar right with
    | VString l, VString r -> VString (l + r)
    | VNull, _ | _, VNull -> VNull
    | VString l, VEmpty -> VString l
    | VEmpty, VString r -> VString r
    | l, r -> arithmeticOf strict OpAdd l r

/// `+` (strict: both operands have declared types, overflow is error 6).
let addWith strict left right =
    match left, right with
    | VInteger l, VInteger r -> integerSum strict left right l r
    | VLong l, VLong r -> longResult strict (int64 l + int64 r)
    | VDouble l, VDouble r -> doubleResult (l + r)
    | VLong l, VInteger r | VInteger l, VLong r -> longResult strict (int64 l + int64 r)
    | VDouble l, (VInteger r | VLong r) | (VInteger r | VLong r), VDouble l -> doubleResult (l + float r)
    | _ -> addSlow strict left right

let subtractWith strict left right =
    match left, right with
    | VInteger l, VInteger r -> integerSum strict left right l (-r)
    | VLong l, VLong r -> longResult strict (int64 l - int64 r)
    | VDouble l, VDouble r -> doubleResult (l - r)
    | VLong l, VInteger r | VInteger l, VLong r -> longResult strict (int64 l - int64 r)
    | VDouble l, (VInteger r | VLong r) -> doubleResult (l - float r)
    | (VInteger l | VLong l), VDouble r -> doubleResult (float l - r)
    | _ ->
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | l, r -> arithmeticOf strict OpSubtract l r

let multiplyWith strict left right =
    match left, right with
    | VInteger l, VInteger r when l >= -32768 && l <= 32767 && r >= -32768 && r <= 32767 ->
        let n = l * r
        if n >= -32768 && n <= 32767 then VInteger n
        elif strict then raiseVbError 6
        else VLong n
    | VLong l, VLong r -> longResult strict (int64 l * int64 r)
    | VDouble l, VDouble r -> doubleResult (l * r)
    | VDouble l, (VInteger r | VLong r) | (VInteger r | VLong r), VDouble l -> doubleResult (l * float r)
    | _ ->
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | l, r -> arithmeticOf strict OpMultiply l r

/// Variant `+`: results that do not fit their type widen.
let addValues left right = addWith false left right
let subtractValues left right = subtractWith false left right
let multiplyValues left right = multiplyWith false left right

/// `/`: a Double, or a Single when one operand is a Single and the other a
/// Single, Integer, Byte or Boolean. 0 / 0 is error 6, x / 0 error 11.
let divideWith strict left right =
    match left, right with
    | VDouble l, VDouble r when r <> 0.0 -> doubleResult (l / r)
    | _ ->
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | l, r ->
        let lr = rankOf l
        let rr = rankOf r
        let x = toNumber l
        let y = toNumber r
        if y = 0.0 then raiseVbError (if x = 0.0 then 6 else 11)
        let singleOperand rank = rank = RByte || rank = RInteger || rank = RSingle || rank = REmpty
        if (lr = RSingle || rr = RSingle) && singleOperand lr && singleOperand rr then singleResult strict (x / y)
        else doubleResult (x / y)

/// `\` and `Mod`: the operands are rounded to whole numbers; the result is a
/// Byte for two Bytes, an Integer for Integers (and Booleans), else a Long.
let integerDivideWith strict modulus left right =
    match left, right with
    | VInteger a, VInteger b when b > 0 && b <= 32767 && a >= -32768 && a <= 32767 -> VInteger (if modulus then a % b else a / b)
    | (VLong a | VInteger a), (VLong b | VInteger b) when b <> 0 && b <> -1 && (match left, right with VInteger _, VInteger _ -> false | _ -> true) ->
        // Long operands (the common case): no overflow is possible.
        VLong (if modulus then a % b else a / b)
    | _ ->
    match scalar left, scalar right with
    | VNull, _ | _, VNull -> VNull
    | l, r ->
        let rank operand =
            match operand with
            | VByte _ -> RByte
            | VInteger n when n >= -32768 && n <= 32767 -> RInteger
            | VBoolean _ | VEmpty -> RInteger
            | _ -> RLong
        let lr = rank l
        let rr = rank r
        let resultRank = if lr = RByte && rr = RByte then RByte elif lr <= RInteger && rr <= RInteger then RInteger else RLong
        let a = int64 (toInteger l)
        let b = int64 (toInteger r)
        if b = 0L then raiseVbError 11
        let n = if modulus then a % b else a / b
        // Variant Integer results widen to Long; a Long that overflows is error 6.
        if resultRank = RLong && not (fits32 n) then raiseVbError 6
        wholeResult strict resultRank n

/// Unary minus.
let negateWith strict value =
    match value with
    | VInteger n when n > -32768 && n <= 32767 -> VInteger (-n)
    | VLong n when n <> Int32.MinValue -> VLong (-n)
    | VDouble d -> VDouble (-d)
    | _ ->
    match scalar value with
    | VNull -> VNull
    | VEmpty -> VInteger 0
    | VByte b -> VInteger (-(int b))
    | VBoolean b -> VInteger (if b then 1 else 0)
    | VInteger n as v -> wholeResult strict (rankOf v) (-(int64 n))
    | VLong n -> wholeResult strict RLong (-(int64 n))
    | VSingle f -> VSingle (-f)
    | VDouble d -> VDouble (-d)
    | VCurrency c -> if c = Int64.MinValue then raiseVbError 6 else VCurrency (-c)
    | VDate d -> checkedDate (-d)
    | other -> VDouble (-(toNumber other))

let negateValue value = negateWith false value

/// `Not`: bitwise on numbers, logical on Booleans.
let notValue value =
    match scalar value with
    | VNull -> VNull
    | VBoolean b -> VBoolean (not b)
    | VByte b -> VByte (byte (~~~(int b) &&& 255))
    | VInteger n when n >= -32768 && n <= 32767 -> VInteger (~~~n)
    | VEmpty -> VInteger -1
    | other -> VLong (~~~(toInteger other))

type LogicalOp = LAnd | LOr | LXor | LEqv | LImp

/// And, Or, Xor, Eqv and Imp: logical on two Booleans, bitwise otherwise (a
/// Byte for two Bytes, an Integer for Integers and Booleans, else a Long).
let logicalWith (op: LogicalOp) left right =
    let l = scalar left
    let r = scalar right
    let truth (v: Value) = toBool v
    match l, r with
    | VBoolean a, VBoolean b ->
        VBoolean (match op with
                  | LAnd -> a && b | LOr -> a || b | LXor -> a <> b
                  | LEqv -> a = b | LImp -> not a || b)
    | VNull, VNull -> VNull
    | VNull, other | other, VNull ->
        let nullOnLeft = (match l with VNull -> true | _ -> false)
        match op with
        | LAnd -> if truth other then VNull else VBoolean false
        | LOr -> if truth other then VBoolean true else VNull
        | LXor | LEqv -> VNull
        | LImp ->
            // Null Imp True is True; False Imp Null is True; otherwise Null.
            if nullOnLeft then (if truth other then VBoolean true else VNull)
            elif truth other then VNull else VBoolean true
    | _ ->
        let rank operand =
            match operand with
            | VByte _ -> RByte
            | VInteger n when n >= -32768 && n <= 32767 -> RInteger
            | VBoolean _ | VEmpty -> RInteger
            | _ -> RLong
        let resultRank = max (rank l) (rank r)
        let a = toInteger l
        let b = toInteger r
        let n =
            match op with
            | LAnd -> a &&& b | LOr -> a ||| b | LXor -> a ^^^ b
            | LEqv -> ~~~(a ^^^ b) | LImp -> (~~~a) ||| b
        match resultRank with
        | RByte -> VByte (byte (n &&& 255))
        | RInteger -> VInteger n
        | _ -> VLong n

/// The start value of a Variant `For` counter, in the widest type of the
/// start, end and step (`For k = 1 To n` with n a Long counts in Longs).
let forCounterStart (start: Value) (finish: Value) (step: Value) =
    let start = scalar start
    let ranks = [ start; scalar finish; scalar step ] |> List.map (fun v -> match v with VEmpty -> REmpty | VNull -> REmpty | other -> rankOf other)
    let rank = ranks |> List.fold max REmpty
    if rank = rankOf start || rank = REmpty then start
    else
        match rank with
        | RByte | RInteger | RLong ->
            let n = wholeOf start
            if fitsRank rank n then wholeOfRank rank n else start
        | RSingle -> singleResult false (toNumber start)
        | RDouble -> VDouble (toNumber start)
        | RCurrency -> VCurrency (toCurrencyUnits start)
        | _ -> start

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
    | VSingle l, VSingle r -> VInteger (compare l r)
    | VSingle l, r -> VInteger (compare l (toSingle (toNumber r)))
    | l, VSingle r -> VInteger (compare (toSingle (toNumber l)) r)
    | l, r -> VInteger (compare (toNumber l) (toNumber r))

let roundedInteger minimum maximum value =
    let rounded = Math.Round(toNumber value, 0, MidpointRounding.ToEven)
    checkedInt minimum maximum rounded

/// Int and Fix keep the operand's type (a Boolean or Empty gives an Integer,
/// a String a Double).
let private wholePart (round: float -> float) value =
    match scalar value with
    | VNull -> VNull
    | (VInteger _ | VLong _ | VByte _) as whole -> whole
    | VBoolean b -> VInteger (if b then -1 else 0)
    | VEmpty -> VInteger 0
    | VSingle f -> VSingle (float32 (round (float f)))
    | VDate d -> VDate (round d)
    | VCurrency c ->
        if round 0.5 = 0.0 && round -0.5 = -1.0 then VCurrency (int64 (Math.Floor(float c / currencyScale)) * 10000L)
        else VCurrency ((c / 10000L) * 10000L)
    | other -> VDouble (round (toNumber other))

let floorValue value = wholePart Math.Floor value
let truncateValue value = wholePart Math.Truncate value

/// Abs keeps the operand's type; Abs of the most negative Integer or Long
/// overflows.
let absValue value =
    match scalar value with
    | VNull -> VNull
    | VEmpty -> VInteger 0
    | VByte _ as b -> b
    | VBoolean b -> VInteger (if b then 1 else 0)
    | VInteger n when n >= -32768 && n <= 32767 -> if n = -32768 then raiseVbError 6 else VInteger (abs n)
    | VInteger n | VLong n -> if n = Int32.MinValue then raiseVbError 6 else VLong (abs n)
    | VSingle f -> VSingle (abs f)
    | VDouble d -> VDouble (abs d)
    | VCurrency c -> if c = Int64.MinValue then raiseVbError 6 else VCurrency (abs c)
    | VDate d -> VDate (abs d)
    | other -> VDouble (abs (toNumber other))

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
    | DeclaredType.Integer -> VInteger 0
    | DeclaredType.Long | DeclaredType.LongLong -> VLong 0
    | DeclaredType.Byte -> VByte 0uy
    | DeclaredType.Single -> VSingle 0.0f
    | DeclaredType.Double -> VDouble 0.0
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
    if fitsInt rounded then VLong (int rounded) else VDouble rounded

/// CSng: a Single, error 6 beyond the Single range.
let toSingleValue value =
    match scalar value with
    | VSingle _ as s -> s
    | v ->
        let x = toNumber v
        let s = toSingle x
        if Single.IsInfinity s || Single.IsNaN s then raiseVbError 6
        VSingle s

/// Convert a value assigned to a variable of a declared type.
let coerceTo (t: DeclaredType) (value: Value) =
    match t, value with
    | DeclaredType.Long, VLong _ -> value
    | DeclaredType.Long, VInteger n -> VLong n
    | DeclaredType.Long, VByte n -> VLong (int n)
    | DeclaredType.Double, (VInteger n | VLong n) -> VDouble (float n)
    | DeclaredType.Integer, VInteger n when n >= -32768 && n <= 32767 -> value
    | DeclaredType.Double, VDouble _ -> value
    | DeclaredType.Single, VSingle _ -> value
    | DeclaredType.Byte, VByte _ -> value
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
    | DeclaredType.Long, v -> VLong (roundedInteger -2147483648.0 2147483647.0 v)
    | DeclaredType.LongLong, v -> toLongLong v
    | DeclaredType.Byte, v -> VByte (byte (roundedInteger 0.0 255.0 v))
    | DeclaredType.Single, v -> toSingleValue v
    | DeclaredType.Double, v -> VDouble (toNumber v)
    | DeclaredType.Currency, v -> VCurrency (toCurrencyUnits v)
    | DeclaredType.Date, v -> VDate (toDate v)
    | DeclaredType.String, v -> VString (toString v)
    | DeclaredType.Boolean, v -> VBoolean (toBool v)
    | _, v -> v
