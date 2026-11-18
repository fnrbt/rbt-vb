/// Times the programs in Programs.fs. Native:
///   dotnet run --project tests/bench/Rbt.Vb.Bench.fsproj -c Release -- [reps] [name ...]
/// Fable / Node:
///   dotnet fable tests/bench/Rbt.Vb.Bench.fsproj --outDir <dir>/bench --extension .mjs
///   node <dir>/bench/Program.mjs [reps] [name ...]
/// Prints the best and median wall time of `reps` runs (after one warm-up)
/// and the program's Debug.Print output, which must match across runtimes.
module Rbt.Vb.Bench.Program

open Rbt.Vb
open Rbt.Vb.Api
open Rbt.Vb.Bench.Programs

/// Milliseconds from an arbitrary origin.
let private now () : float =
#if FABLE_COMPILER
    Fable.Core.JsInterop.emitJsExpr () "performance.now()"
#else
    float (System.Diagnostics.Stopwatch.GetTimestamp()) * 1000.0 / float System.Diagnostics.Stopwatch.Frequency
#endif

let private runOnce (program: BenchProgram) =
    let output = ResizeArray<string>()
    let hooks = { Api.defaultHooks with Print = fun text -> output.Add text }
    let options = ProjectOptions.vba |> ProjectOptions.withHooks hooks
    let modules =
        program.Modules
        |> List.map (fun (name, isClass, source) ->
            if isClass then ModuleSource.classModule name source else ModuleSource.standard name source)
    match Project.compile options modules with
    | Error failures -> failwithf "%s: %s" program.Name (failures |> List.map VbFailure.format |> String.concat "; ")
    | Ok project ->
        let started = now ()
        let session = Session.Start project
        match session.RunToEnd() with
        | Failed failure -> failwithf "%s: %s" program.Name (VbFailure.format failure)
        | _ -> ()
        match session.Run(program.Entry, []) with
        | Failed failure -> failwithf "%s: %s" program.Name (VbFailure.format failure)
        | _ -> ()
        now () - started, String.concat " | " output

let private runtime =
#if FABLE_COMPILER
    "fable"
#else
    "dotnet"
#endif

let run (argv: string array) =
    let reps, names =
        match argv |> Array.toList with
        | first :: rest when first |> Seq.forall System.Char.IsDigit -> int first, rest
        | rest -> 5, rest
    let selected = all |> List.filter (fun p -> names.IsEmpty || List.contains p.Name names)
    let mutable sum = 0.0
    for program in selected do
        ignore (runOnce program)
        let times = Array.init reps (fun _ -> runOnce program)
        let sorted = times |> Array.map fst |> Array.sort
        let best = sorted.[0]
        let median = sorted.[sorted.Length / 2]
        sum <- sum + median
        printfn "%-7s %-13s best %7.0f ms  median %7.0f ms  out: %s" runtime program.Name best median (snd times.[0])
    printfn "%-7s %-13s median sum %7.0f ms" runtime "total" sum

#if FABLE_COMPILER
run (
    let argv: string array = Fable.Core.JsInterop.emitJsExpr () "process.argv.slice(2)"
    argv)
#else
[<EntryPoint>]
let main argv = run argv; 0
#endif
