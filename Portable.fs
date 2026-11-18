/// Idioms for code that runs on .NET and, compiled by Fable, as JavaScript.
///
/// Rbt.Vb ships to both. F# that is fast on .NET can be slow in JavaScript
/// (and the other way round), so hot paths (the VM's dispatch loop, operators,
/// array access, calls) follow these rules. Each one compiles to plain code on
/// both backends; measure with tests/bench (see its Program.fs) before and
/// after changing a hot path, on .NET and under Fable 4 (what rbt-office
/// uses) and Fable 5 (the pinned tool).
///
/// 1. Loops: `for i in a .. b` (step 1) and `for i = a downto b` only. A
///    stepped range (`for i in n - 1 .. -1 .. 0`) is an enumerator under
///    Fable 4: an object, MoveNext/Current calls and a try/finally per loop.
///    Count down with `downto`; for other steps use a `while` loop.
///
/// 2. No `int64` in hot paths. In JavaScript an int64 is a BigInt, and every
///    operation allocates. VBA's Integer and Long are 32-bit:
///    - `+` and `-`: add in `int` and test the overflow with the sign trick
///      (`addOverflows`, `subtractOverflows`); in JavaScript Fable truncates
///      (`| 0`) or the bitwise test truncates, so both agree.
///    - `*`, and any wider intermediate: compute in `Wide` (a Double). It
///      holds every sum, difference and quotient of two Int32s exactly; a
///      product is exact whenever it fits 32 bits and is rounded once (as a
///      64-bit product converted to Double would be) when it does not, so the
///      range test `fitsInt32` and a widened Double result are both exact.
///    - Keep `int64` where 64 bits are the value: Currency (VCurrency) and
///      LongLong, date arithmetic, Hex/Oct of large numbers.
///
/// 3. Array reads and writes whose index is already known to be in range
///    (the VM's code, stack, locals, constants and globals; an offset after a
///    bounds check) use `at` / `setAt`. Fable's `arr.[i]` calls a checked
///    `item`/`setItem` helper; these are plain `a[i]` in JavaScript and an
///    ordinary (JIT-checked) access on .NET. Never use them on an index that
///    comes from the program being run without checking it first: in
///    JavaScript an out-of-range read is `undefined`, not an exception.
///
/// 4. Values without fields in hot paths come from a module-level binding
///    (`Values.Shared`): Fable 4 allocates `VEmpty` (`new Value(5, [])`) on
///    every use. `VBoolean true/false` are shared the same way (both
///    backends allocate them otherwise).
///
/// 5. An `inline` helper keeps only its fast path inline and calls a separate
///    function for the rare case (a stack that must grow, an error), so the
///    dispatch function stays small enough for both JITs to optimise.
module Rbt.Vb.Portable

/// A whole number that may not fit 32 bits: an intermediate of Integer/Long
/// arithmetic (see rule 2). A plain number in JavaScript, unlike `int64`.
type Wide = float

/// An Int32 as a Wide (exact).
let inline wide (n: int) : Wide = float n

[<Literal>]
let MinInt32 : Wide = -2147483648.0

[<Literal>]
let MaxInt32 : Wide = 2147483647.0

/// `n` is a whole number in Int32's range.
let inline fitsInt32 (n: Wide) = n >= MinInt32 && n <= MaxInt32

/// `n` is a whole number in Int16's range (VBA's Integer).
let inline fitsInt16 (n: Wide) = n >= -32768.0 && n <= 32767.0

/// The 32-bit `sum = l + r` overflowed (`sum` as computed in `int`).
let inline addOverflows (l: int) (r: int) (sum: int) = ((l ^^^ sum) &&& (r ^^^ sum)) < 0

/// The 32-bit `difference = l - r` overflowed.
let inline subtractOverflows (l: int) (r: int) (difference: int) = ((l ^^^ r) &&& (l ^^^ difference)) < 0

#if FABLE_COMPILER
/// `a.[i]` for an index known to be in range: no bounds check in JavaScript.
[<Fable.Core.Emit("$0[$1]")>]
let at (a: 'T array) (i: int) : 'T = Unchecked.defaultof<'T>

/// `a.[i] <- v` for an index known to be in range.
[<Fable.Core.Emit("$0[$1] = $2")>]
let setAt (a: 'T array) (i: int) (v: 'T) : unit = ()
#else
/// `a.[i]` for an index known to be in range: no bounds check in JavaScript.
let inline at (a: 'T array) (i: int) : 'T = a.[i]

/// `a.[i] <- v` for an index known to be in range.
let inline setAt (a: 'T array) (i: int) (v: 'T) : unit = a.[i] <- v
#endif
