module Bob

open System.Text.RegularExpressions

type Input = Question | Yell | YellAQuestion | SayNothing | Whatever

let toString = function
    | Question -> "Sure."
    | Yell -> "Whoa, chill out!"
    | YellAQuestion -> "Calm down, I know what I'm doing!"
    | SayNothing -> "Fine. Be that way!"
    | Whatever -> "Whatever."

let sanitize (x: string) = x.Trim()

let (|Questioning|_|) (str: string) = 
    match str.Substring(str.Length - 1) with
    | "?" -> Some Question
    | _ -> None

let (|Yelling|_|) (str: string) =
    let m = Regex.Match(str, @"[a-z]")
    if m.Success then None else Some Yell

let (|QuestionWhileYelling|_|) (str: string) =
    let m = Regex.Match(str, @"[a-z]")
    let n = Regex.Match(str, @"[A-Z]")
    if str.Substring(str.Length - 1) = "?" && (not m.Success) && n.Success
    then Some YellAQuestion
    else None

let (|BlankResponse|_|) (str: string) =
    if Regex.Replace(str, @"\s+", "") = ""
    then Some SayNothing
    else None

let response (str: string) =
    let s' = sanitize str
    match s' with
    | BlankResponse x -> toString x
    | QuestionWhileYelling x -> toString x
    | Questioning x -> toString x
    | Yelling x -> toString x
    | _ -> toString Whatever
