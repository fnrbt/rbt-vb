module Rbt.Vb.Tests.FixtureCases

#if !FABLE_COMPILER
open System.IO
open Rbt.Vb
open Rbt.Vb.Values

let private execute filename =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "..", filename)
    match Api.runSource Api.RunOptions.vbScript (File.ReadAllText path) with
    | Ok result -> result.State
    | Error error -> failwithf "%s failed: %A" filename error

let private expect state name expected =
    match Api.tryGetGlobal name state with
    | Some actual when actual = expected -> ()
    | actual -> failwithf "%s: expected %A, got %A" name expected actual

let run () =
    let algorithms = execute "test_real_algorithms.vbs"
    expect algorithms "s" (VString "1,2,3,4,5,6,7,8,8,10,11,15,22,33")
    expect algorithms "fib_result" (VString "0,1,1,2,3,5,8,13,21,34,55,89,144")
    expect algorithms "primes" (VString "2,3,5,7,11,13,17,19,23,29,31,37,41,43,47")
    let classes = execute "test_real_classes.vbs"
    match Api.tryGetGlobal "listArr" classes with
    | Some (VArray array) when array.Data = [| VInteger 10; VInteger 20; VInteger 30 |] -> ()
    | actual -> failwithf "LinkedList.ToArray: expected [10;20;30], got %A" actual
    printfn "PASS existing algorithm and class fixtures"
#else
let run () = ()
#endif
