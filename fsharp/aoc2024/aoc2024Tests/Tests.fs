module Tests

open System
open System.IO
open System.Linq
open System.Text.RegularExpressions
open Xunit
open Swensen.Unquote

let readSample fileName =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "SampleData", fileName)
    File.ReadAllLines inputPath |> Seq.ofArray
    
module Day1 =
    
    let input = [
        (3, 4)
        (4, 3)
        (2, 5)
        (1, 3)
        (3, 9)
        (3, 3)
    ]
       
    let parseLine (line: string) =
       let regex = "(\d+)\s+(\d+)"
       let groups = Regex.Match(line, regex).Groups
       let l = groups[1].Value |> int
       let r = groups[2].Value |> int
       (l, r)
   
    [<Fact>]
    let ``sample data`` () =
        let left, right = input |> List.unzip
        let l = left |> List.sort
        let r = right |> List.sort
        let res = l |> List.mapi (fun i li -> Math.Abs(li - r[i]))
        let result = res |> List.sum
        test <@ result = 11 @>
    
    [<Fact>]
    let ``Part 1`` () =
        let inputs = readSample "day1_1.txt"
           
        let lines = inputs |> Seq.map parseLine |> List.ofSeq
        
        let left, right = lines |> List.unzip
        let l = left |> List.sort
        let r = right |> List.sort
        let res = l |> List.mapi (fun i li -> Math.Abs(li - r[i]))
        let result = res |> List.sum
        test <@ result = 1882714 @>
    
    [<Fact>]
    let ``Part 2 - sample data`` () =
       let left, right = input |> List.unzip
       
       let mutable results = []
       for lItem in left do
           let multiplier = right |> List.filter (fun r -> lItem = r) |> List.length
           let entry = lItem * multiplier
           results <- entry :: results
           
       let result = results.Sum()
       test <@ result = 31 @>
       
    [<Fact>]
    let ``Part 2`` () =
       let left, right = readSample "day1_1.txt" |> Seq.map parseLine |> List.ofSeq |> List.unzip
       
       let mutable results = []
       for lItem in left do
           let multiplier = right |> List.filter (fun r -> lItem = r) |> List.length
           let entry = lItem * multiplier
           results <- entry :: results
           
       let result = results.Sum()
       test <@ result = 19437052 @>
module Day2 =

    let sample =
        [ "7 6 4 2 1"; "1 2 7 8 9"; "9 7 6 2 1"; "1 3 2 4 5"; "8 6 4 4 1"; "1 3 6 7 9" ]

    let parseLineToLevels (line: string) =
        line.Split(' ') |> Array.map (fun i -> i |> int) |> List.ofArray
        
    [<Fact>]
    let ``sample data`` () =
        let reports = sample |> List.map parseLineToLevels

        let isMonotonic (deltas: int list) =
            let isInRange (x: int) = [ 1..3 ].Contains(Math.Abs(x))
            let sign = Math.Sign(deltas[0])
            deltas |> List.forall (fun x -> isInRange x && Math.Sign(x) = sign)

        let isSafe (report: int list) =
            let pairs = report |> List.pairwise
            pairs |> List.map (fun (a, b) -> b - a) |> isMonotonic

        let result = reports |> List.filter isSafe |> List.length
        test <@ result = 2 @>
        
    [<Fact>]
    let ``Part 1`` () =
        let lines = readSample "day2_1.txt" |> List.ofSeq
        let reports = lines |> List.map parseLineToLevels
        
        let isMonotonic (deltas: int list) =
            let isInRange (x: int) = [ 1..3 ].Contains(Math.Abs(x))
            let sign = Math.Sign(deltas[0])
            deltas |> List.forall (fun x -> isInRange x && Math.Sign(x) = sign)

        let isSafe (report: int list) =
            let pairs = report |> List.pairwise
            pairs |> List.map (fun (a, b) -> b - a) |> isMonotonic

        let result = reports |> List.filter isSafe |> List.length
        test <@ result = 516 @>
        
    [<Fact>]
    let ``Part 2 - sample data`` () =
        let reports = sample |> List.map parseLineToLevels

        let isMonotonic (deltas: int list) =
            let isInRange (x: int) = [ 1..3 ].Contains(Math.Abs(x))
            let sign = Math.Sign(deltas[0])
            deltas |> List.forall (fun x -> isInRange x && Math.Sign(x) = sign)

        let isSafe (report: int list) =
            let pairs = report |> List.pairwise
            let traditionalSafe = pairs |> List.map (fun (a, b) -> b - a) |> isMonotonic
            
            if traditionalSafe then
                true
            else
                let mutable results = []
                let l = (report |> List.length) - 1
                let range = [0..l]
                for i in range do
                    let tolerantPairs = report |> List.removeAt i |> List.pairwise
                    if tolerantPairs |> List.map (fun (a, b) -> b - a) |> isMonotonic then
                        results <- true :: results
                    else
                        results <- false :: results
                results |> List.contains true

        let result = reports |> List.filter isSafe |> List.length
        test <@ result = 4 @>
        
    [<Fact>]
    let ``Part 2`` () =
        let reports = readSample "day2_1.txt" |> List.ofSeq |> List.map parseLineToLevels

        let isMonotonic (deltas: int list) =
            let isInRange (x: int) = [ 1..3 ].Contains(Math.Abs(x))
            let sign = Math.Sign(deltas[0])
            deltas |> List.forall (fun x -> isInRange x && Math.Sign(x) = sign)

        let isSafe (report: int list) =
            let traditionalSafe = report |> List.pairwise |> List.map (fun (a, b) -> b - a) |> isMonotonic
            
            if traditionalSafe then
                true
            else
                let maybeMonotonic =
                    let maxIndex = (report |> List.length) - 1
                    [0..maxIndex]
                    |> List.tryFind (fun index ->
                        report
                        |> List.removeAt index
                        |> List.pairwise
                        |> List.map (fun (a, b) -> b - a) |> isMonotonic)
                match maybeMonotonic with
                | Some _ -> true
                | _ -> false

        let result = reports |> List.filter isSafe |> List.length
        test <@ result = 561 @>
        