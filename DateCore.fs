/// Calendar arithmetic for VB Date values.
///
/// A VB Date is an OLE Automation date: a Double counting days from
/// 1899-12-30 00:00, with the time of day as the fraction. Before the epoch
/// the integer part counts days backwards and the fraction still runs forward
/// (-1.25 is 1899-12-29 06:00). Serial numbers of Excel's 1900 date system are
/// the same numbers from 1900-03-01 on. Everything here is integer arithmetic,
/// so .NET and JavaScript agree; parsing and names follow en-US.
module Rbt.Vb.DateCore

/// 100-01-01 and 9999-12-31 23:59:59, the range of VB dates.
let minSerial = -657434.0
let maxSerial = 2958465.99999

let private floorDiv (a: int) (b: int) = if a >= 0 then a / b else (a - b + 1) / b

/// Days since 1970-01-01 of a proleptic Gregorian date (H. Hinnant's algorithm).
let daysFromCivil (year: int) (month: int) (day: int) =
    let y = if month <= 2 then year - 1 else year
    let era = floorDiv y 400
    let yoe = y - era * 400
    let mp = (month + 9) % 12
    let doy = (153 * mp + 2) / 5 + day - 1
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097 + doe - 719468

let civilFromDays (days: int) =
    let z = days + 719468
    let era = floorDiv z 146097
    let doe = z - era * 146097
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    let y = yoe + era * 400
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    let mp = (5 * doy + 2) / 153
    let d = doy - (153 * mp + 2) / 5 + 1
    let m = if mp < 10 then mp + 3 else mp - 9
    (if m <= 2 then y + 1 else y), m, d

let private epochOffset = 25569 // 1970-01-01 as a VB date

let isLeapYear year = (year % 4 = 0 && year % 100 <> 0) || year % 400 = 0

let daysInMonth year month =
    match month with
    | 2 -> if isLeapYear year then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | _ -> 31

/// Day number (VB serial of midnight) of a calendar date.
let dayNumber year month day = daysFromCivil year month day + epochOffset

/// A continuous day count from an OLE date (negative dates count forward
/// through the day) and back.
let toLinear (serial: float) =
    if serial >= 0.0 then serial
    else
        let day = System.Math.Truncate serial
        day - (serial - day) // day + |fraction|, with day < 0 and fraction negative

let ofLinear (linear: float) =
    if linear >= 0.0 then linear
    else
        let day = System.Math.Floor linear
        let fraction = linear - day
        if fraction = 0.0 then day else day - fraction

/// (year, month, day, hour, minute, second) of a date, rounded to the second.
let components (serial: float) =
    let linear = toLinear serial
    let mutable day = int (System.Math.Floor linear)
    let mutable seconds = int (System.Math.Round((linear - System.Math.Floor linear) * 86400.0))
    if seconds >= 86400 then
        seconds <- seconds - 86400
        day <- day + 1
    let y, m, d = civilFromDays (day - epochOffset)
    y, m, d, seconds / 3600, (seconds / 60) % 60, seconds % 60

let datePart (serial: float) = int (System.Math.Floor (toLinear serial + 0.5 / 86400.0))

let timeSeconds (serial: float) =
    let y, m, d, h, n, s = components serial
    ignore (y, m, d)
    h * 3600 + n * 60 + s

/// The OLE date of a calendar date and time (the time may be any number of seconds).
let fromComponents year month day (seconds: float) =
    ofLinear (float (dayNumber year month day) + seconds / 86400.0)

/// Two-digit years as VB reads them: 0-29 are 2000-2029, 30-99 are 1930-1999.
let expandYear (year: int) =
    if year >= 0 && year < 30 then 2000 + year
    elif year >= 30 && year < 100 then 1900 + year
    else year

/// DateSerial: months and days outside their range roll over.
let dateSerial (year: int) (month: int) (day: int) =
    let year = expandYear year
    let monthIndex = year * 12 + (month - 1)
    let y = floorDiv monthIndex 12
    let m = monthIndex - y * 12 + 1
    float (dayNumber y m 1 + day - 1)

/// Weekday 1..7 of a date counted from `firstDay` (1 = Sunday; 0 = system = Sunday).
let weekday (serial: float) (firstDay: int) =
    let firstDay = if firstDay = 0 then 1 else firstDay
    let day = datePart serial
    // 1899-12-30 (day 0) was a Saturday: Sunday-based weekday 7.
    let sundayBased = ((day + 6) % 7 + 7) % 7 + 1
    ((sundayBased - firstDay + 7) % 7) + 1

/// The first day of week 1 of `year` (1 vbFirstJan1, 2 vbFirstFourDays, 3 vbFirstFullWeek).
let private weekOneStart (year: int) (firstDay: int) (firstWeek: int) =
    let jan1 = dayNumber year 1 1
    let wd = weekday (float jan1) firstDay
    match firstWeek with
    | 2 -> if wd <= 4 then jan1 - (wd - 1) else jan1 + (8 - wd)
    | 3 -> if wd = 1 then jan1 else jan1 + (8 - wd)
    | _ -> jan1 - (wd - 1)

/// The week of the year (DatePart "ww"), counted from `firstDay`.
let weekOfYear (serial: float) (firstDay: int) (firstWeek: int) =
    let day = datePart serial
    let y, _, _ = civilFromDays (day - epochOffset)
    let start = weekOneStart y firstDay firstWeek
    let start = if day < start then weekOneStart (y - 1) firstDay firstWeek else start
    (day - start) / 7 + 1

let monthNames = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |]
let dayNames = [| "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday" |]
let abbreviation (name: string) = name.Substring(0, 3)

let private pad2 (n: int) = if n < 10 then "0" + string n else string n

/// "1/31/2024"
let shortDate (serial: float) =
    let y, m, d, _, _, _ = components serial
    string m + "/" + string d + "/" + string y

/// "1:45:00 PM"
let longTime (serial: float) =
    let _, _, _, h, n, s = components serial
    let h12 = if h % 12 = 0 then 12 else h % 12
    string h12 + ":" + pad2 n + ":" + pad2 s + (if h < 12 then " AM" else " PM")

/// How VB prints a Date (CStr, Debug.Print, the general date format): the
/// date, the time, or both, omitting a zero date and a midnight time.
let generalDate (serial: float) =
    let hasTime = timeSeconds serial <> 0
    let hasDate = datePart serial <> 0
    if hasDate && hasTime then shortDate serial + " " + longTime serial
    elif hasDate then shortDate serial
    else longTime serial

let isValidSerial (serial: float) =
    not (System.Double.IsNaN serial) && serial >= minSerial && serial <= maxSerial + 1e-9

// ── Parsing ──

type private DateToken =
    | Num of int * int   // value, digit count
    | Word of string
    | Sep of char

let private tokenizeDate (text: string) =
    let tokens = ResizeArray<DateToken>()
    let mutable i = 0
    let mutable ok = true
    while ok && i < text.Length do
        let c = text.[i]
        if c >= '0' && c <= '9' then
            let start = i
            while i < text.Length && text.[i] >= '0' && text.[i] <= '9' do i <- i + 1
            if i - start > 9 then ok <- false
            else tokens.Add(Num (int (text.Substring(start, i - start)), i - start))
        elif System.Char.IsLetter c then
            let start = i
            while i < text.Length && System.Char.IsLetter text.[i] do i <- i + 1
            tokens.Add(Word (text.Substring(start, i - start).ToLowerInvariant()))
        elif c = '/' || c = '-' || c = '.' || c = ',' || c = ':' then
            tokens.Add(Sep c)
            i <- i + 1
        elif c = ' ' || c = '\t' then
            tokens.Add(Sep ' ')
            i <- i + 1
        else ok <- false
    if ok then Some (List.ofSeq tokens) else None

let private monthOfWord (word: string) =
    if word.Length < 3 then None
    else
        monthNames |> Array.tryFindIndex (fun name -> name.ToLowerInvariant().StartsWith word)
        |> Option.map (fun i -> i + 1)

let private isWeekdayWord (word: string) =
    word.Length >= 3 && dayNames |> Array.exists (fun name -> name.ToLowerInvariant().StartsWith word)

/// Parse a date and/or time the way CDate reads en-US text: "1/31/2024",
/// "2024-01-31", "Jan 31, 2024", "31 January 2024", "13:45", "1:45:30 PM",
/// "1/31/2024 1:45 PM", "2024-01-31T13:45:00". Returns the OLE date.
let tryParse (text: string) (currentYear: int) : float option =
    let text = if isNull text then "" else text.Trim()
    if text = "" then None
    else
    match tokenizeDate text with
    | None -> None
    | Some tokens ->
        let dateNumbers = ResizeArray<int * int>()
        let mutable month = None
        let mutable time = None
        let mutable ok = true
        let mutable rest = tokens
        let skipSpaces toks = toks |> List.skipWhile (fun t -> t = Sep ' ')
        let ampm toks =
            match skipSpaces toks with
            | Word ("am" | "a") :: tail -> Some false, tail
            | Word ("pm" | "p") :: tail -> Some true, tail
            | _ -> None, toks
        let setTime h n s (pm: bool option) =
            let h =
                match pm with
                | Some isPm ->
                    if h < 1 || h > 12 then ok <- false
                    (h % 12) + (if isPm then 12 else 0)
                | None -> h
            if h > 23 || n > 59 || s > 59 || time.IsSome then ok <- false
            time <- Some (h * 3600 + n * 60 + s)
        while ok && not rest.IsEmpty do
            match rest with
            | Num (h, _) :: Sep ':' :: Num (n, _) :: Sep ':' :: Num (s, _) :: tail ->
                // Optional fraction of a second.
                let tail = match tail with Sep '.' :: Num _ :: t -> t | t -> t
                let pm, tail = ampm tail
                setTime h n s pm
                rest <- tail
            | Num (h, _) :: Sep ':' :: Num (n, _) :: tail ->
                let pm, tail = ampm tail
                setTime h n 0 pm
                rest <- tail
            | Num (h, _) :: tail when (match ampm tail with Some _, _ -> true | _ -> false) ->
                let pm, tail = ampm tail
                setTime h 0 0 pm
                rest <- tail
            | Num (v, digits) :: tail ->
                dateNumbers.Add((v, digits))
                rest <- tail
            | Word "t" :: tail -> rest <- tail
            | Word w :: tail ->
                match monthOfWord w with
                | Some m when month.IsNone -> month <- Some m; rest <- tail
                | _ when isWeekdayWord w -> rest <- tail
                | _ -> ok <- false
            | Sep _ :: tail -> rest <- tail
            | [] -> ()
        if not ok then None
        else
            let valid y m d = y >= 100 && y <= 9999 && m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth y m
            let date =
                let nums = List.ofSeq dateNumbers
                match month, nums with
                | None, [] -> Some None
                | None, [ (a, da); (b, _) ] ->
                    if da >= 3 then Some (Some (a, b, 1))           // yyyy-mm
                    elif b > 31 then Some (Some (expandYear b, a, 1)) // m/yyyy
                    else Some (Some (currentYear, a, b))            // m/d
                | None, [ (a, da); (b, _); (c, dc) ] ->
                    if da >= 3 then Some (Some (a, b, c))          // yyyy-mm-dd
                    else
                        let y = if dc <= 2 then expandYear c else c
                        // m/d/y, or d/m/y when the first number cannot be a month.
                        if a > 12 && b <= 12 then Some (Some (y, b, a)) else Some (Some (y, a, b))
                | Some m, [] -> Some (Some (currentYear, m, 1))
                | Some m, [ (a, da) ] ->
                    if da >= 3 || a > 31 then Some (Some (expandYear a, m, 1)) else Some (Some (currentYear, m, a))
                | Some m, [ (a, da); (b, db) ] ->
                    if da >= 3 || a > 31 then Some (Some (a, m, b))
                    else Some (Some ((if db <= 2 then expandYear b else b), m, a))
                | _ -> None
            match date with
            | None -> None
            | Some dateParts ->
                let seconds = float (defaultArg time 0)
                match dateParts with
                | None ->
                    if time.IsNone then None else Some (seconds / 86400.0)
                | Some (y, m, d) ->
                    if valid y m d then Some (fromComponents y m d seconds) else None
