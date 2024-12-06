module Tests

open System
open System.IO
open System.Linq
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open Xunit
open Swensen.Unquote

let readSample fileName =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "SampleData", fileName)
    File.ReadAllLines inputPath |> Seq.ofArray

module Day1 =

    let input = [ (3, 4); (4, 3); (2, 5); (1, 3); (3, 9); (3, 3) ]

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
        let left, right =
            readSample "day1_1.txt" |> Seq.map parseLine |> List.ofSeq |> List.unzip

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
                let range = [ 0..l ]

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
            let maybeMonotonic =
                let maxIndex = (report |> List.length) - 1

                [ 0..maxIndex ]
                |> List.tryFind (fun index ->
                    report
                    |> List.removeAt index
                    |> List.pairwise
                    |> List.map (fun (a, b) -> b - a)
                    |> isMonotonic)

            match maybeMonotonic with
            | Some _ -> true
            | _ -> false

        let result = reports |> List.filter isSafe |> List.length
        test <@ result = 561 @>

module Day3 =

    let calcInstructions instructions =
        instructions
        |> Seq.map (fun i ->
            let entry = i |> string
            let regex2 = "(\d+),(\d+)"
            let g = Regex.Match(entry, regex2).Groups
            let a = g[1].Value |> int
            let b = g[2].Value |> int
            (a * b))
        |> Seq.sum

    let regexMul = "mul\((\d{1,3})\,(\d{1,3})\)"

    [<Fact>]
    let ``Part 1 - sample`` () =
        let input =
            "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

        let result = Regex.Matches(input, regexMul) |> calcInstructions

        test <@ result = 161 @>

    [<Fact>]
    let ``Part 1`` () =
        let input = readSample "day3_1.txt" |> String.concat ""
        let result = Regex.Matches(input, regexMul) |> calcInstructions

        test <@ result = 173785482 @>

    let getEnabled (input: string) =
        input.Split("do()")
        |> List.ofArray
        |> List.filter (fun el -> el <> "")
        |> List.map (fun el ->
            if el.Contains("don't()") then
                el.Substring(0, el.IndexOf("don't()"))
            else
                el)
        |> String.concat ""

    [<Fact>]
    let ``Part 2 - sample`` () =
        let inputRaw =
            "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

        let input = "do()" + inputRaw

        let enabled = getEnabled input

        let result = Regex.Matches(enabled, regexMul) |> calcInstructions

        test <@ result = 48 @>

    [<Fact>]
    let ``Part 2`` () =
        let inputRaw = readSample "day3_1.txt" |> String.concat ""
        let input = "do()" + inputRaw

        let enabled = getEnabled input

        let result = Regex.Matches(enabled, regexMul) |> calcInstructions

        test <@ result = 83158140 @>

module Day4 =

    let csToString (cs: char list) =
        let sb = StringBuilder(cs.Length)
        cs |> List.iter (sb.Append >> ignore)
        sb.ToString()

    let reverseString (s: string) = s |> Seq.rev |> Seq.toArray |> String

    // Idea:
    // - rows and columns can be searched with regex.
    // - Diagonals are treated differently.
    //      -> I'll use Array2D and some matrix stuff.
    [<Fact>]
    let ``Part 1 - sample`` () =
        let sampleInput =
            [ "MMMSXXMASM"
              "MSAMXMSMSA"
              "AMXSXMAAMM"
              "MSAMASMSMX"
              "XMASAMXAMM"
              "XXAMMXXAMA"
              "SMSMSASXSS"
              "SAXAMASAAA"
              "MAMMMXMMMM"
              "MXMXAXMASX" ]

        let transposeMatrix (matrix: char array2d) =
            let length = Array2D.length1 matrix
            let transposed = Array2D.create length length '.'

            for x in [ 0 .. (length - 1) ] do
                for y in [ 0 .. (length - 1) ] do
                    transposed[y, x] <- matrix[x, y]

            transposed

        let matrix2Lists (matrix: char array2d) =
            let mutable result = []
            let length = (Array2D.length1 matrix) - 1

            for r in [ 0..length ] do
                let s = matrix[r, *] |> String
                result <- s :: result

            result |> List.rev

        let getDiagonals (input: string list) =

            let inputMatrix = array2D input

            // Assumption: The matrix has the same height and width
            let getMainDiagonal (a: char array2d) =
                let length = Array2D.length1 a
                let mutable diagonal = []

                for i in 0 .. (length - 1) do
                    diagonal <- a[i, i] :: diagonal

                diagonal |> List.rev

            let getUpperRightDiagonals (originalMatrix: char array2d) =
                let length = (Array2D.length1 originalMatrix) - 1
                let mutable allDiagonals = []

                for i in [ 0..length ] do
                    let reversedIndex = length - i
                    let diagonal = originalMatrix[0..reversedIndex, i..length] |> getMainDiagonal
                    allDiagonals <- diagonal :: allDiagonals

                allDiagonals

            let diagsUpperRight = inputMatrix |> getUpperRightDiagonals
            // Remove the first entry, because we already have that from the previous step (`List.rev |> List.tail`)
            let diagsLowerLeft =
                inputMatrix
                |> transposeMatrix
                |> getUpperRightDiagonals
                |> List.rev
                |> List.tail

            let allDiagonals = diagsUpperRight @ diagsLowerLeft
            let result = allDiagonals |> List.map (fun x -> x |> csToString)
            result

        let diagonalsLeftToRight = sampleInput |> getDiagonals
        let diagonalsRightToLeft = sampleInput |> List.map reverseString |> getDiagonals
        let diagonals = diagonalsLeftToRight @ diagonalsRightToLeft

        let countMatches (pattern: string) (s: string) =
            let reversedPattern = pattern |> reverseString
            let forwardMatchCount = Regex.Matches(s, pattern).Count
            let backwardMatchCount = Regex.Matches(s, reversedPattern).Count
            forwardMatchCount + backwardMatchCount

        let countXmas (ss: string list) =
            ss |> List.map (countMatches "XMAS") |> List.sum

        let diagonalMatchCount = diagonals |> countXmas
        let rowMatchCount = sampleInput |> countXmas

        let columnMatchCount =
            sampleInput |> array2D |> transposeMatrix |> matrix2Lists |> countXmas

        let result = diagonalMatchCount + rowMatchCount + columnMatchCount

        test <@ result = 18 @>
        
    [<Fact>]
    let ``Part 1`` () =
        let sampleInput = readSample "day4_1.txt" |> List.ofSeq

        let transposeMatrix (matrix: char array2d) =
            let length = Array2D.length1 matrix
            let transposed = Array2D.create length length '.'

            for x in [ 0 .. (length - 1) ] do
                for y in [ 0 .. (length - 1) ] do
                    transposed[y, x] <- matrix[x, y]

            transposed

        let matrix2Lists (matrix: char array2d) =
            let mutable result = []
            let length = (Array2D.length1 matrix) - 1

            for r in [ 0..length ] do
                let s = matrix[r, *] |> String
                result <- s :: result

            result |> List.rev

        let getDiagonals (input: string list) =

            let inputMatrix = array2D input

            // Assumption: The matrix has the same height and width
            let getMainDiagonal (a: char array2d) =
                let length = Array2D.length1 a
                let mutable diagonal = []

                for i in 0 .. (length - 1) do
                    diagonal <- a[i, i] :: diagonal

                diagonal |> List.rev

            let getUpperRightDiagonals (originalMatrix: char array2d) =
                let length = (Array2D.length1 originalMatrix) - 1
                let mutable allDiagonals = []

                for i in [ 0..length ] do
                    let reversedIndex = length - i
                    let diagonal = originalMatrix[0..reversedIndex, i..length] |> getMainDiagonal
                    allDiagonals <- diagonal :: allDiagonals

                allDiagonals

            let diagsUpperRight = inputMatrix |> getUpperRightDiagonals
            // Remove the first entry, because we already have that from the previous step (`List.rev |> List.tail`)
            let diagsLowerLeft =
                inputMatrix
                |> transposeMatrix
                |> getUpperRightDiagonals
                |> List.rev
                |> List.tail

            let allDiagonals = diagsUpperRight @ diagsLowerLeft
            let result = allDiagonals |> List.map (fun x -> x |> csToString)
            result

        let diagonalsLeftToRight = sampleInput |> getDiagonals
        let diagonalsRightToLeft = sampleInput |> List.map reverseString |> getDiagonals
        let diagonals = diagonalsLeftToRight @ diagonalsRightToLeft

        let countMatches (pattern: string) (s: string) =
            let reversedPattern = pattern |> reverseString
            let forwardMatchCount = Regex.Matches(s, pattern).Count
            let backwardMatchCount = Regex.Matches(s, reversedPattern).Count
            forwardMatchCount + backwardMatchCount

        let countXmas (ss: string list) =
            ss |> List.map (countMatches "XMAS") |> List.sum

        let diagonalMatchCount = diagonals |> countXmas
        let rowMatchCount = sampleInput |> countXmas

        let columnMatchCount =
            sampleInput |> array2D |> transposeMatrix |> matrix2Lists |> countXmas

        let result = diagonalMatchCount + rowMatchCount + columnMatchCount

        test <@ result = 18 @> // 2434 too low