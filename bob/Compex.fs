module Compex

open System.Text.RegularExpressions
open FsUnit

type Input = Question | Yell | YellAQuestion | SayNothing | Whatever

let toString = function
    | Question -> "Sure."
    | Yell -> "Whoa, chill out!"
    | YellAQuestion -> "Calm down, I know what I'm doing!"
    | SayNothing -> "Fine. Be that way!"
    | Whatever -> "Whatever."

let sanitize (x: string) = x.Trim()

let questioning (str: string) = 
    if str.Length = 0 then None else
    match str.Substring(str.Length - 1) with
    | "?" -> Some Question
    | _ -> None

let yelling (str: string) =
    let m = Regex.Match(str, @"[a-z]")
    if m.Success then None else Some Yell

let questionWhileYelling (str: string) =
    if str.Length = 0 then None else
    let m = Regex.Match(str, @"[a-z]")
    let n = Regex.Match(str, @"[A-Z]")
    if str.Substring(str.Length - 1) = "?" && (not m.Success) && n.Success
    then Some YellAQuestion
    else None

let blankResponse (str: string) =
    if Regex.Replace(str, @"\s+", "") = ""
    then Some SayNothing
    else None

type OrElseBuilder() =
    member this.Return(x) = x

    member this.Combine(a, b) =
        match a with
        | Some _ -> a
        | None -> b

    member this.Delay(f) = f()

let orElse = new OrElseBuilder()

let response (str: string) =
    let s' = sanitize str
    let result = 
        orElse {
            return blankResponse s'
            return questionWhileYelling s'
            return questioning s'
            return yelling s'
        }
    let str = match result with
    | Some x -> toString x
    | None -> toString Whatever
    printfn "%s" str
    str
