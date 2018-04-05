module compex2

let strToInt str =
    let (res, value) = System.Int32.TryParse(str)
    if res then Some value else None

let (>>=) m f =
    match m with
    | Some a -> f a
    | None -> f 0

let strAdd str i =
    let num = strToInt str
    match num with
    | Some x -> Some (x + i)
    | None -> Some i

let unwrap x =
    match x with
    | Some a -> a
    | None -> 0

let a = (strToInt "10" >>= strAdd "xyz" >>= strAdd "5") |> unwrap
