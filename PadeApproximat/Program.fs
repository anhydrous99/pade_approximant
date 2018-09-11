// Learn more about F# at http://fsharp.org

open System
open Pade

[<EntryPoint>]
let main argv =
    let taylor = [|1.0;1.0;0.5;0.166667;0.0416667;0.00833333;0.00138889;0.000198413;0.0000248016|]
    let pad = pade taylor
    printfn "%A" pad.Num
    printfn "%A" pad.Den
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code