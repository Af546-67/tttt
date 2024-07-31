type Coach = {
    Name: string
    FormerPlayer: bool
}

type Stats = {
    Wins: int
    Losses: int
}

type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}

let teams = [
    {
        Name = "Boston Celtics"
        Coach = { Name = "Joe Mazzulla"; FormerPlayer = false }
        Stats = { Wins = 64; Losses = 18 }
    }
    {
        Name = "Golden State Warriors"
        Coach = { Name = "Steve Kerr"; FormerPlayer = true }
        Stats = { Wins = 46; Losses = 36 }
    }
    {
        Name = "Miami Heat"
        Coach = { Name = "Erik Spoelstra"; FormerPlayer = false }
        Stats = { Wins = 46; Losses = 36 }
    }
    {
        Name = "Chicago Bulls"
        Coach = { Name = "Billy Donovan"; FormerPlayer = false }
        Stats = { Wins = 39; Losses = 43 }
    }
    {
        Name = "Denver Nuggets"
        Coach = { Name = "Michael Malone"; FormerPlayer = false }
        Stats = { Wins = 57; Losses = 25 }
    }
]

let calculateWinLossPercentages team =
    let totalGames = float (team.Stats.Wins + team.Stats.Losses)
    let winPercentage = (float team.Stats.Wins / totalGames) * 100.0
    let lossPercentage = (float team.Stats.Losses / totalGames) * 100.0
    (team.Name, team.Coach.Name, team.Stats.Wins, team.Stats.Losses, winPercentage, lossPercentage)

let successfulTeams =
    teams
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

let teamPercentages = 
    successfulTeams
    |> List.map calculateWinLossPercentages

printfn "Successful Teams and their Coaches:"
printfn "Team Name\t\tCoach Name\t\tWins\tLosses\tWin Percentage\tLoss Percentage"
teamPercentages
|> List.iter (fun (teamName, coachName, wins, losses, winPercentage, lossPercentage) -> 
    printfn "%s\t\t%s\t\t%d\t%d\t%.2f%%\t\t%.2f%%" teamName coachName wins losses winPercentage lossPercentage)


type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget activity =
    match activity with
    | BoardGame -> 0.0, 0
    | Chill -> 0.0, 0
    | Movie movieType ->
        match movieType with
        | Regular -> 12.0, 0
        | IMAX -> 17.0, 0
        | DBOX -> 20.0, 0
        | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks -> 
            let basePrice =
                match movieType with
                | RegularWithSnacks -> 12.0
                | IMAXWithSnacks -> 17.0
                | DBOXWithSnacks -> 20.0
                | _ -> 0.0
            basePrice + 5.0, 0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0, 0
        | Turkish -> 65.0, 0
    | LongDrive (kilometers, fuelCostPerKm) ->
        let cost = float kilometers * fuelCostPerKm
        cost, kilometers

let exampleActivities = [
    BoardGame
    Chill
    Movie Regular
    Movie IMAX
    Movie DBOX
    Movie RegularWithSnacks
    Restaurant Korean
    Restaurant Turkish
    LongDrive (100, 1.25)
]

exampleActivities
|> List.iter (fun activity ->
    match calculateBudget activity with
    | cost, kilometers when kilometers > 0 ->
        printfn "Activity: LongDrive (%d km), Cost: %.2f CAD" kilometers cost
    | cost, _ ->
        printfn "Activity: %A, Cost: %.2f CAD" activity cost)
