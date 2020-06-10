module Tests

open System
open System
open Xunit
open Npgsql.FSharp
open Microsoft.FSharp.Reflection
open FsUnit.Xunit

let ofOption error = function Some s -> Ok s | None -> Error error

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = new ResultBuilder()

let defaultConnection  =
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"

type Event = {
    Id: int
    Type: string
    Body: string
}

let getEvents (fromId: int) : Result<Event list, exn> =
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

    
type AccountCreatedEvent = {
    AccountId: int
    Name: string }

type AccountCreditedEvent = {
    AccountId: int
    Amount: decimal }

type Events =
    | AccountCreated of AccountCreatedEvent
    | AccountCredited of AccountCreditedEvent
    
let createEvent (o: Object) =
    if o :? AccountCreatedEvent then (downcast o |> AccountCreated |> Some)
    else if o:? AccountCreditedEvent then (downcast o |> AccountCreated |> Some)
    else None


type Reducer<'State, 'Event> = 'State -> 'Event -> 'State

type Computed<'State, 'Derived> = 'State -> 'Derived

type HandlerResult<'Event> = Result<'Event, string>

type Handler<'State, 'Command, 'Event> = 'State -> 'Command -> HandlerResult<'Event>


module Result =
    let parseInt (str: string) =
        match Int32.TryParse(str) with
        | true, i -> i |> Ok
        | _ -> sprintf "Failed to parse int from %s" str |> Error
        
    let parseDecimal (str: string) =
        match Decimal.TryParse(str) with
        | true, i -> i |> Ok
        | _ -> sprintf "Failed to parse int from %s" str |> Error


type ShortString = private ShortString of string

module ShortString =
    let create str =
        if String.IsNullOrEmpty(str) then Error("Value can not be null or empty")
        else if String.length str >= 100 then Error("Value must be less than 100 characters")
        else ShortString str |> Ok
    let toString (ShortString s) = s
    
    
type Id = private Id of int

module Id =
    let create i =
        if i < 1 then "Value must be a positive integer" |> Error
        else Id i |> Ok
    let toInt (Id i) = i

module Map =
    let update: 'a -> ('b -> 'b) -> Map<'a, 'b> -> Map<'a, 'b> = fun key update map ->
        match Map.tryFind key map with
        | Some(value) -> Map.add key (update value) map
        | None -> map
        
    let findResult key = Map.tryFind key >> function | Some(v) -> Ok(v) | None -> Error (sprintf "Key %s not found" key)

type Dto =
    | Object of Map<string, Dto>
    | Value of string
    
type Serializer<'a> = 'a -> Dto
type Deserializer<'a> =  Dto -> Result<'a, string>

type Dto<'a> = Serializer<'a> * Deserializer<'a>

module Dto =
    
    let getValue = function | Object(map) -> Error "Is map" | Value(str) -> Ok str
    let getMap = function | Object(map) -> Ok map| Value(str) -> Error "Is value"
    
    let toInt: Deserializer<int> = fun dto -> result {
        let! value = getValue dto
        let! i = Result.parseInt value
        return i
    }
    
    let fromInt: Serializer<int> = fun i -> i.ToString() |> Dto.Value
        
    let toDecimal: Deserializer<decimal> = fun dto -> result {
        let! value = getValue dto
        let! i = Result.parseDecimal value
        return i
    }
    
    let fromDecimal: Serializer<decimal> = fun d -> d.ToString() |> Dto.Value
    
    let get key (binder: Dto -> Result<'a, string>) dto = result {
        let! map = getMap dto
        let! value = Map.findResult key map
        let! bound = binder value
        return bound
    }
    
    let ofSeq (s: (string * Dto) seq) = Map.ofSeq s |> Dto.Object
    
    let rec toJson = function
        | Object(map) ->
            Map.map (fun k v -> sprintf "\"%s\":%s" k (toJson v)) map
            |> Map.toSeq
            |> Seq.map snd
            |> String.concat "," |> sprintf "{%s}"
        | Value(s) -> sprintf "\"%s\"" s
    
    open System.Text.Json
    open System.Collections.Generic
    open System.Linq
    
    let fromJson (str: string) =
        let dictionary: Dictionary<string, string> = JsonSerializer.Deserialize<Dictionary<string, string>>(str)
        dictionary.Select(fun kv -> (kv.Key, kv.Value))
        |> Map.ofSeq
        |> Map.map (fun k v -> Dto.Value v)
        |> Dto.Object
        
        
type Amount = private Amount of decimal

module Amount =
    let create d =
        if d <= 1_000_000_000m then sprintf "Value %f is out of range" d |> Error
        else if d >= 1_000_000_000m then sprintf "Value %f is out of range" d |> Error 
        else Amount d |> Ok
        
module Account =
    type CreatedAccountEvent = { Id: int; }
    type CreditedAccountEvent = { Id: int; Amount: decimal }
    type DebitedAccountEvent = { Id: int; Amount: decimal }
    type TransferredFundsEvent = { FromId: int; ToId: int; Amount: decimal; }
    type ClosedAccountEvent = { Id: int }
    
    type Event =
        | Created of CreatedAccountEvent
        | Credited of CreditedAccountEvent
        | Debited of DebitedAccountEvent
        | Transferred of TransferredFundsEvent
        | Closed of ClosedAccountEvent
    
    let private getId = Dto.get "Id" Dto.toInt
    let private getAmount = Dto.get "Amount" Dto.toDecimal 
    
    let deserialize: Dto -> string -> Result<Event, string> = fun dto -> function
        | "Created" -> (result {
            let! id = getId dto
            return Created({ Id = id })
        })
        | "Credited" -> (result {
            let! id = getId dto
            let! amount = getAmount dto
            return Credited({ Id = id; Amount = amount })
        })
        | "Debited" -> (result {
            let! id = getId dto
            let! amount = getAmount dto
            return Debited({ Id = id; Amount = amount })
        })
        | "Transferred" -> (result {
            let! amount = getAmount dto
            let! toId = Dto.get "ToId" Dto.toInt dto
            let! fromId = Dto.get "FromId" Dto.toInt dto 
            return Transferred({ Amount = amount; FromId = fromId; ToId = toId })
        })
        | "Closed" -> (result {
            let! id = getId dto
            return Closed({ Id = id })
        })
        | _ -> Error("Not supported")
        
    let serialize: Event -> Dto = (
        function
        | Created e -> [("Id", Dto.fromInt e.Id)]
        | Credited e -> [("Id", Dto.fromInt e.Id); ("Amount", Dto.fromDecimal e.Amount)]
        | Debited e -> [("Id", Dto.fromInt e.Id); ("Amount", Dto.fromDecimal e.Amount)]
        | Transferred e -> [("FromId", Dto.fromInt e.FromId); ("ToId", Dto.fromInt e.ToId); ("Amount", Dto.fromDecimal e.Amount)]
        | Closed e -> [("Id", Dto.fromInt e.Id)]) >> Dto.ofSeq

    module GetBalance =
        let name = "GetBalance"
        
        type State = Map<int, decimal>
        
        let private transfer e =
            let update id balance =
                match id with
                | id when id = e.FromId -> balance - e.Amount
                | id when id = e.ToId -> balance + e.Amount
                | _ -> balance
            Map.map update
        
        let reducer: Reducer<State, Event> = fun state ->
            function
            | Created e -> Map.add e.Id 0m state
            | Credited e -> Map.update e.Id ((+) e.Amount) state
            | Debited e -> Map.update e.Id ((-) e.Amount) state
            | Transferred e -> transfer e state
            | Closed e -> Map.remove e.Id state
            
        let empty: State = Map.empty
                
    module GetOverdrawn =
        let name = "GetOverdrawn"
        
        type State = Set<int>
        
        let computed: Computed<GetBalance.State, State> =
            Map.filter (fun id balance -> balance < 0m)
            >> Map.toSeq
            >> Seq.map fst
            >> Set.ofSeq
            
        let empty: State = Set.empty
            
    module GetNextId =
        let name = "GetNextId"
        
        type State = int
        
        let reducer: Reducer<State, Event> = fun state ->
            function
            | Created e -> state + 1
            | Credited e -> state
            | Debited e -> state
            | Transferred e -> state
            | Closed e -> state
            
        let empty: State = 0
    
    module GetActive =
        let name = "GetActive"
        
        type State = Set<int>
        
        let reducer: Reducer<State, Event> = fun state ->
            function
            | Created e -> Set.add e.Id state
            | Credited e -> state
            | Debited e -> state
            | Transferred e -> state
            | Closed e -> Set.remove e.Id state
            
        let empty: State = Set.empty
    
    type State = {
        Active: GetActive.State
        Balances: GetBalance.State
        NextId: GetNextId.State }
    
    let empty: State = {
        Active = GetActive.empty
        Balances = GetBalance.empty
        NextId = GetNextId.empty }
    
    type Id = int
    
    let getBalance id (state: State) =
        match Map.tryFind id state.Balances with
        | Some(balance) -> balance |> Ok
        | None -> Error "Account id not found"
        
    let getActiveAccount id (state: State) =
        if Set.contains id state.Active
        then Ok(id)
        else Error("Account id not found")

    let reducer: Reducer<State, Event> = fun state event ->
        { state with
            Balances = GetBalance.reducer state.Balances event }
        
module Overdraft =
    type Event =
        | Extended of int * decimal
        | ChargedFee of int * decimal
        
    type State = Map<int, decimal>
    
module AccountManager =
    type TransferFundsCommand = {
        FromAccountId: Account.Id
        ToAccountId: Account.Id
        Amount: decimal }
    
    type Command =
        | OpenAccount
        | TransferFunds of TransferFundsCommand
        | CloseAccount of Account.Id
        with
            static member from (o: Object) = None 
        
    type State = Account.State * Overdraft.State
    
    let private openAccount id = Account.Event.Created { Id = id } |> Ok
    
    let private transferFunds command accounts = result {
        let! fromBalance = Account.getBalance command.FromAccountId accounts
        let! toBalance = Account.getBalance command.ToAccountId accounts
        
        if command.Amount >= fromBalance then
            return! Error("Not enough funds")
        else
            return Account.Event.Transferred { FromId = command.FromAccountId; ToId = command.ToAccountId; Amount = command.Amount }
    }
    
    let private closeAccount id state = result {
        let! account = Account.getActiveAccount id state
        return Account.Event.Closed { Id = account }
    }
    
    let handler: Handler<State, Command, Account.Event> = fun (accounts, overdrafts) -> function
        | OpenAccount -> openAccount accounts.NextId
        | TransferFunds id -> transferFunds id accounts
        | CloseAccount id -> closeAccount id accounts
            
type Customer = {
    Id: int
    Name: string
    Age: int }

type CreditFile = {
    CustomerId: int
    Score: float }

type State = {
    Accounts:  Map<int, int> }

type Reducer = State -> Object -> State

type Reducer<'Event> = State -> 'Event -> State

type Constructor<'a> = Object -> 'a option

let mapReducer: Reducer<'a> -> Constructor<'a> -> Reducer = fun reducer construct state event ->
    match (construct event) with
    | Some(e) -> reducer state e
    | None -> state

let reducer: Reducer<Events> = fun (state: State) ->
    function
    | AccountCreated(a) -> state
    | AccountCredited(a) -> state

[<Fact>]
let ``Serialize / deserialize test`` () =
    let event = Account.Event.Credited({ Id = 1; Amount = 1.23m })
    let dto = Account.serialize event
    let json = Dto.toJson dto
    json |> should equal @"{""Amount"":""1.23"",""Id"":""1""}"
    let dtob = Dto.fromJson json
    dtob |> should equal dto
    let eventb = Account.deserialize dtob "Credited"
    eventb |> should equal (Result<Account.Event, string>.Ok(event))
    