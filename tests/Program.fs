module Rbt.Vb.Tests.Program

[<EntryPoint>]
let main _ =
    let suites = [
        "public API", RegressionCases.runAll
        "value semantics", SemanticCases.run
        "0.3.0 release", ReleaseCases.runAll
        "0.4.0 release", Release040Cases.runAll
        "0.5.0 release", Release050Cases.runAll
#if !FABLE_COMPILER
        "persistence", PersistenceCases.run
        "existing fixtures", FixtureCases.run
#endif
    ]
    let mutable failures = []
    for name, suite in suites do
        try
            suite ()
            printfn "PASS suite: %s" name
        with error ->
            failures <- (name + ": " + error.Message) :: failures
            eprintfn "FAIL suite: %s: %s" name error.Message
    if failures.IsEmpty then 0
    else
#if FABLE_COMPILER
        failwith (failures |> List.rev |> String.concat "\n")
#else
        1
#endif
