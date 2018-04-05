module SumOfMultiples

let sum set upperBound =
    let folder acc num =
        let rec folder' counter multiples =
            if num >= upperBound then [] else
            let next = num * counter
            if next >= upperBound then multiples
            else folder' (counter + 1) (multiples @ [next])
        acc @ folder' 1 []

    set |> ((List.fold folder []) >> List.distinct >> List.sum)


// let sum4 (numbers: int list) (upperBound: int): int = 
//         numbers 
//         |> Seq.collect (fun n -> { n .. n .. upperBound-1 })
//         |> Seq.distinct
//         |> Seq.sum

// let sum (numbers: int list) (upperBound: int): int = 
//     seq {0..upperBound-1}
//     |> Seq.filter (fun n -> numbers |> List.exists(fun ll -> n % ll = 0))
//     |> Seq.sum
