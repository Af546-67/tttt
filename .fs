
let productOfOdds n =
    let rec productHelper acc current =
        if current < 1 then acc
        else productHelper (acc * current) (current - 2)
    productHelper 1 n


let oddNumber = 11
printfn "Product of odd numbers from %d to 1: %d" oddNumber (productOfOdds oddNumber)

