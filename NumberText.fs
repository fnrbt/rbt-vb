/// Decimal digits of Doubles, shared by CStr, Str, Format and FormatNumber.
///
/// Numbers are handled as a significant-digit string and a decimal exponent,
/// read from the shortest round-trip text that both .NET and JavaScript
/// produce, so the two runtimes print the same digits.
module Rbt.Vb.NumberText

/// Significant digits (no leading or trailing zeros) and exponent `e` with
/// |x| = 0.d1d2d3... × 10^e. Zero is ("", 0).
/// The Single nearest to `x` (round half to even), as `float32 x` on .NET.
/// Fable does not round `float32` conversions, so this rounds explicitly.
let toSingle (x: float) : float32 =
#if FABLE_COMPILER
    if x = 0.0 || System.Double.IsNaN x || System.Double.IsInfinity x then float32 x
    else
        let a = abs x
        if a >= 3.4028235677973366E38 then float32 (if x > 0.0 then infinity else -infinity)
        else
            let mutable e = int (System.Math.Floor(System.Math.Log(a) / System.Math.Log(2.0)))
            if System.Math.Pow(2.0, float e) > a then e <- e - 1
            if System.Math.Pow(2.0, float (e + 1)) <= a then e <- e + 1
            let ulp = System.Math.Pow(2.0, float ((max e -126) - 23))
            let q = System.Math.Round(a / ulp, System.MidpointRounding.ToEven)
            let r = q * ulp
            float32 (if x < 0.0 then -r else r)
#else
    float32 x
#endif


type Digits = { Digits: string; Exponent: int }

let zeroDigits = { Digits = ""; Exponent = 0 }

let private trimDigits (digits: string) (exponent: int) =
    let mutable start = 0
    while start < digits.Length && digits.[start] = '0' do start <- start + 1
    let mutable stop = digits.Length
    while stop > start && digits.[stop - 1] = '0' do stop <- stop - 1
    if start = stop then zeroDigits
    else { Digits = digits.Substring(start, stop - start); Exponent = exponent - start }

/// Digits of a decimal text such as "123.45", "1E-07", "1e+21" (sign ignored).
let ofText (text: string) =
    let text = text.Trim().TrimStart('-', '+')
    let mantissa, exp10 =
        match text.IndexOfAny [| 'e'; 'E' |] with
        | -1 -> text, 0
        | i -> text.Substring(0, i), int (text.Substring(i + 1))
    let intPart, fracPart =
        match mantissa.IndexOf '.' with
        | -1 -> mantissa, ""
        | i -> mantissa.Substring(0, i), mantissa.Substring(i + 1)
    trimDigits (intPart + fracPart) (intPart.Length + exp10)

let ofFloat (x: float) =
    if x = 0.0 || System.Double.IsNaN x || System.Double.IsInfinity x then zeroDigits
    else ofText (string (abs x))

/// Digits of an integer count of 1/10^scale units (Currency is scale 4).
let ofScaled (units: int64) (scale: int) =
    let text = string (if units < 0L then -units else units)
    trimDigits text (text.Length - scale)

/// Round to `count` significant digits, half away from zero.
let roundSignificant (d: Digits) (count: int) =
    if d.Digits.Length <= count then d
    elif count <= 0 then
        if count = 0 && d.Digits.[0] >= '5' then { Digits = "1"; Exponent = d.Exponent + 1 } else zeroDigits
    else
        let kept = d.Digits.Substring(0, count).ToCharArray()
        if d.Digits.[count] >= '5' then
            let mutable i = count - 1
            let mutable carry = true
            while carry && i >= 0 do
                if kept.[i] = '9' then
                    kept.[i] <- '0'
                    i <- i - 1
                else
                    kept.[i] <- char (int kept.[i] + 1)
                    carry <- false
            if carry then trimDigits ("1" + System.String(kept)) (d.Exponent + 1)
            else trimDigits (System.String(kept)) d.Exponent
        else trimDigits (System.String(kept)) d.Exponent

/// Round to `decimals` digits after the decimal point, half away from zero.
let roundDecimals (d: Digits) (decimals: int) =
    if d.Digits = "" then d else roundSignificant d (d.Exponent + decimals)

/// The digits of the integer part ("0" when there is none).
let integerPart (d: Digits) =
    if d.Digits = "" || d.Exponent <= 0 then "0"
    elif d.Exponent >= d.Digits.Length then d.Digits + System.String('0', d.Exponent - d.Digits.Length)
    else d.Digits.Substring(0, d.Exponent)

/// The first `decimals` digits after the decimal point (padded with zeros).
let fractionPart (d: Digits) (decimals: int) =
    if decimals <= 0 then ""
    else
        let sb = System.Text.StringBuilder()
        for i in 0 .. decimals - 1 do
            let index = d.Exponent + i // position in Digits of the (i+1)th decimal
            sb.Append(if index >= 0 && index < d.Digits.Length then d.Digits.[index] else '0') |> ignore
        sb.ToString()

/// Digits after the decimal point without trailing zeros.
let significantFraction (d: Digits) =
    if d.Digits = "" then ""
    elif d.Exponent >= d.Digits.Length then ""
    elif d.Exponent >= 0 then d.Digits.Substring(d.Exponent)
    else System.String('0', -d.Exponent) + d.Digits

let private exponentText (e: int) =
    (if e < 0 then "E-" else "E+") + (let a = abs e in if a < 10 then "0" + string a else string a)

/// The text of a magnitude with at most `precision` significant digits, as
/// VB prints Doubles (precision 15) and Singles (7): fixed notation when the
/// integer part has fewer than `precision` digits and at most `precision`
/// digits follow the decimal point ("0.0000000001", "0.333333333333333"),
/// otherwise scientific ("1E+15", "3.33333333333333E-04").
let general (d: Digits) (precision: int) =
    let d = roundSignificant d precision
    if d.Digits = "" then "0"
    else
        let x = d.Exponent - 1
        if x < precision && d.Digits.Length - x - 1 <= precision then
            let fraction = significantFraction d
            integerPart d + (if fraction = "" then "" else "." + fraction)
        else
            let rest = d.Digits.Substring 1
            d.Digits.Substring(0, 1) + (if rest = "" then "" else "." + rest) + exponentText x

/// How VB prints a Double (CStr, Debug.Print, string concatenation): 15
/// significant digits, so 0.1 + 0.2 prints "0.3".
let ofDouble (x: float) =
    if System.Double.IsNaN x then "NaN"
    elif System.Double.IsPositiveInfinity x then "1.#INF"
    elif System.Double.IsNegativeInfinity x then "-1.#INF"
    else
        let text = general (ofFloat x) 15
        if x < 0.0 && text <> "0" then "-" + text else text

/// The decimal digits of a Single: 7 significant digits.
let ofSingleDigits (x: float32) = roundSignificant (ofFloat (float x)) 7

/// How VB prints a Single: 7 significant digits (CStr(Rnd) is "0.9078093").
let ofSingle (x: float32) =
    let f = float x
    if System.Double.IsNaN f then "NaN"
    elif System.Double.IsInfinity f then (if f > 0.0 then "1.#INF" else "-1.#INF")
    else
        let text = general (ofFloat f) 7
        if f < 0.0 && text <> "0" then "-" + text else text

let private digitsValue (d: Digits) =
    if d.Digits = "" then 0.0
    else
        let text = "0." + d.Digits + "E" + string d.Exponent
#if FABLE_COMPILER
        System.Double.Parse text
#else
        System.Double.Parse(text, System.Globalization.CultureInfo.InvariantCulture)
#endif

/// The Double with a Single's 7 printed digits (CCur(CSng(0.1)) is 0.1).
let singleAsDouble (x: float32) =
    let v = digitsValue (ofSingleDigits x)
    if x < 0.0f then -v else v

/// How VB prints a Currency: up to four decimals, no trailing zeros.
let ofCurrency (units: int64) =
    let d = ofScaled units 4
    let fraction = significantFraction d
    let text = integerPart d + (if fraction = "" then "" else "." + fraction)
    if units < 0L then "-" + text else text

/// Insert thousands separators into a run of digits.
let groupThousands (digits: string) =
    if digits.Length <= 3 then digits
    else
        let sb = System.Text.StringBuilder()
        let first = digits.Length % 3
        if first > 0 then sb.Append(digits.Substring(0, first)) |> ignore
        let mutable i = first
        while i < digits.Length do
            if sb.Length > 0 then sb.Append ',' |> ignore
            sb.Append(digits.Substring(i, 3)) |> ignore
            i <- i + 3
        sb.ToString()
