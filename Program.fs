// Learn more about F# at http://fsharp.org
#light

module hw07

open System

let explodes s =
    Seq.toList s


let implode L =
    let sb = System.Text.StringBuilder()
    L |> List.iter (fun c -> ignore (sb.Append (c:char)))
    sb.ToString()


let rec length str =
    match str with
    | [] -> 0
    | e::rest -> 1 + length rest


let rec countVowels str =
    match str with
    | [] -> 0
    | e::rest when e = 'a' || e = 'e' || e = 'o' || e = 'i' || e = 'u' -> 1 + countVowels rest
    | e::rest -> 0 + countVowels rest


let rec countEachVowel str vowel =
    match str with
    | [] -> 0
    | e::rest when e = vowel -> 1 + countEachVowel rest vowel
    | e::rest -> 0 + countEachVowel rest vowel


let rec rotateVowel str =
    match str with 
    | [] -> []
    | e::rest when e = 'a' -> 'e' :: rotateVowel rest
    | e::rest when e = 'e' -> 'i' :: rotateVowel rest
    | e::rest when e = 'i' -> 'o' ::  rotateVowel rest
    | e::rest when e = 'o' -> 'u' ::  rotateVowel rest
    | e::rest when e = 'u' -> 'a' ::  rotateVowel rest
    | e::rest -> e :: rotateVowel rest
    
    
    
[<EntryPoint>]
let main argv =
    printf "input> "
    let input = System.Console.ReadLine()
    
    let listInput = explodes input
    let listInputLen = length listInput
    let toVowels = countVowels listInput
    
    let countA = countEachVowel listInput 'a'
    let countE = countEachVowel listInput 'e'
    let countI = countEachVowel listInput 'i'
    let countO = countEachVowel listInput 'o'
    let countU = countEachVowel listInput 'u'
    let rotated = rotateVowel listInput
    let toStr = implode rotated
    
    printfn "length: %A" listInputLen
    printfn "vowels: %A"  toVowels
    printfn "a: %A" countA
    printfn "e: %A" countE
    printfn "i: %A" countI
    printfn "o: %A" countO
    printfn "u: %A" countU
    printfn "rotated: %A" toStr
    
    0// return an integer exit code
