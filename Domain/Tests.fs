module Tests

open Xunit
open FsUnit.Xunit
open Shared
open Accounts    
open AccountManager

[<Fact>]
let ``AccountManager deserialize`` () =
    let data: (CommandDto * Command * Account.State) list = [
        (
            {Type = Dto.OpenAccountCommandType; Body = @"{""Type"": ""asdf""}" },
            Command.OpenAccount,
            Account.State.empty
        )
        (
            {Type = Dto.TransferFundsCommandType; Body = @"{""Type"": ""asdf"", ""FromAccountId"": 1, ""ToAccountId"": 2, ""Amount"": 10.0}" },
            Command.TransferFunds ({ FromAccountId = AccountId.cast 1; ToAccountId = AccountId.cast 2; Amount = BalanceAmount.cast 10.0m}),
            {Account.State.empty with Active = Set.ofSeq [1; 2]}
        )
        (
            {Type = Dto.CloseAccountCommandType; Body = @"{""Type"": ""asdf"", ""Id"": 1}" },
            Command.CloseAccount(1 |> AccountId.cast),
            {Account.State.empty with Active = Set.ofSeq [1]}
        )
    ]
    
    for (dto, cmd, state) in data do
        let actual = Dto.deserialize state dto |> Result.unwrap
        actual |> should equal cmd

let run commands =
    let rec loop state commands events errors = 
        match commands with
        | c::tail ->
            match _handler state c with
            | Ok(e) ->
                let nextState = Account._reducer state e
                loop nextState tail (events @ [e]) errors
            | Error(e) -> loop state tail events (errors @ [e])
        | [] -> state, events, errors
        
    loop Account.State.empty commands [] []
    
[<Fact>]
let ``AccountManager handler`` () =
    let commands: Command list = [
        Command.OpenAccount
        Command.OpenAccount
        Command.TransferFunds({FromAccountId = 1 |> AccountId.cast; ToAccountId = AccountId.cast 2; Amount = BalanceAmount.cast 0m})
    ]
    
    let expectedEvents: Account.Event list = [
        Account.Event.Created ({AccountId = IntId.cast 1})
        Account.Event.Created ({AccountId = IntId.cast 2}) ]
    
    let expectedState: Account.State = {
        Account.State.empty with
            Active = Set.ofSeq [1; 2]
            Balances = Map.ofSeq [(1, 0m); (2, 0m)]
            NextId = 3 }
    
    let (state, events, errors) = run commands
    
    events |> should equal expectedEvents
    state |> should equal expectedState
    errors |> should equal ["Not enough funds"]