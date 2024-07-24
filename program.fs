let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let calculateTax salary =
    let s = float salary
    match s with
    | s when s <= 49020.0 -> s * 0.15
    | s when s <= 98040.0 -> s * 0.205
    | s when s <= 151978.0 -> s * 0.26
    | s when s <= 216511.0 -> s * 0.29
    | _ -> s * 0.33

let highIncomeSalaries = salaries |> List.filter (fun salary -> salary > 100000)
printfn "High-Income Salaries: %A" highIncomeSalaries

let taxes = salaries |> List.map calculateTax
printfn "Taxes: %A" taxes

let updatedSalaries = salaries |> List.map (fun salary -> if salary < 49020 then salary + 20000 else salary)
printfn "Updated Salaries (with $20,000 added to those below $49,020): %A" updatedSalaries

let sumOfSalariesInRange =
    updatedSalaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (fun acc salary -> acc + salary) 0
printfn "Sum of Salaries between $50,000 and $100,000: %d" sumOfSalariesInRange

let sumOfMultiplesOf3 n =
    let rec helper current acc =
        if current > n then acc
        else helper (current + 3) (acc + current)
    helper 3 0

let result = sumOfMultiplesOf3 27
printfn "The sum of all multiples of 3 up to 27 is %d" result
