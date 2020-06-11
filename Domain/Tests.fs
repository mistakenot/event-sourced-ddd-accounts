module Tests

open System
open System
open System.ComponentModel.DataAnnotations
open System.ComponentModel.DataAnnotations
open Xunit
open Npgsql.FSharp
open Microsoft.FSharp.Reflection
open FsUnit.Xunit
open Shared
open Accounts

let defaultConnection  =
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"

type EventDto = {
    Id: int
    Type: string
    Body: string
}

let getEvents (fromId: int) : Result<EventDto list, exn> =
    defaultConnection
        |> Sql.connectFromConfig
        |> Sql.query "select get_events(@fromId);"
        |> Sql.parameters ["fromId", Sql.int fromId]
        |> Sql.execute (fun read -> 
            {
                Id = read.int "id"
                Type = read.text "type"
                Body = read.text "body"
            })

let putEvent (previousId: int) (eventType: string) (body: string) : Result<int, exn> =
    defaultConnection
        |> Sql.connectFromConfig
        |> Sql.query "select put_event(@previousId, @type, @payload);"
        |> Sql.parameters [
            ("previousId", Sql.int previousId)
            ("type", Sql.text eventType)
            ("payload", Sql.jsonb body)
        ]
        |> Sql.executeNonQuery

// [<Fact>]
let ``My test`` () =
    let xx = result {
        let! putResult = putEvent 0 "eventtype" "{}"
        Assert.Equal(putResult, -1)
        return putResult
    }
    match xx with
    | Ok(x) -> Assert.Equal(x, -1)
    | Error(e) -> raise e

open Newtonsoft.Json.Schema.Generation

// [<Fact>]
let ``Nuget`` () =
    let generator = JSchemaGenerator()
    let schema = generator.Generate(typeof<Account.Dto.ClosedAccountEvent>)
    schema |> should equal ""
    
open AccountManager
open Accounts
open Accounts.AccountManager

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