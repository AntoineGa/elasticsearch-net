#I @"../../packages/build/FAKE/tools"
#r @"FakeLib.dll"
open System
open Fake 
open System.IO
open Microsoft.FSharp.Reflection

module Unions = 

    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let toSeq<'a> = 
        FSharpType.GetUnionCases(typeof<'a>)
        |> Seq.map (fun case -> case.Name)

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None
