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
