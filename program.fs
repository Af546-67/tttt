
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]


let highIncomeSalaries = salaries |> List.filter (fun salary -> salary > 100000)
printfn "High-Income Salaries: %A" highIncomeSalaries


let calculateTax salary =
    let s = float salary  
    match s with
    | s when s <= 49020.0 -> s * 0.15
    | s when s <= 98040.0 -> s * 0.205
    | s when s <= 151978.0 -> s * 0.26
    | s when s <= 216511.0 -> s * 0.29
    | _ -> s * 0.33


let taxes = salaries |> List.map calculateTax
printfn "Salaries with Taxes: %A" (List.zip salaries taxes)


let updatedSalaries = 
    salaries
    |> List.filter (fun salary -> salary < 49020)
    |> List.map (fun salary -> salary + 20000)
printfn "Updated Salaries (less than $49,020 with $20,000 added): %A" updatedSalaries


let sumInRange =
    salaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0
printfn "Sum of Salaries between $50,000 and $100,000: %d" sumInRange


let productOfEven n =
    let rec helper current acc =
        if current <= 0 then
            acc
        else
            helper (current - 2) (acc * current)
    
    
    helper n 1


let evenTarget = 10
let productResult = productOfEven evenTarget
printfn "The product of all even numbers from %d to 2 is %d" evenTarget productResult


let sumOfMultiplesOf3UpTo target =
   
    let rec sumHelper current sum multiples =
        if current > target then
            (sum, multiples)
        else
            sumHelper (current + 3) (sum + current) (multiples @ [current])
    
    
    sumHelper 3 0 []


let multipleTarget = 27
let (sumResult, multiples) = sumOfMultiplesOf3UpTo multipleTarget


printfn "Multiples of 3 up to %d: %A" multipleTarget multiples
printfn "Sum of multiples of 3 up to %d is: %d" multipleTarget sumResult
printfn "Detailed Calculation: %s = %d" (multiples |> List.map string |> String.concat "+") sumResult
