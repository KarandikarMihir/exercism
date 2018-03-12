module Bob

type Input = Question | Yell | YellAQuestion | SayNothing | Whatever

let toString = function
    | Question -> "Sure."
    | Yell -> "Whoa, chill out!"
    | YellAQuestion -> "Calm down, I know what I'm doing"
    | SayNothing -> "Fine. Be that way"
    | Whatever -> "Whatever"

let (|Questioning|_|) (str: string) = 
    Some Question

let sanitize (x: string) = x.Trim()

let response (str: string) =
    match sanitize str with
    | Questioning x ->
        match x with
        | r -> toString r
    | _ -> toString Whatever
