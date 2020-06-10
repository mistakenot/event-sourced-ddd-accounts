module Accounts

open System.ComponentModel.DataAnnotations

open Shared
        
type BalanceAmount = private Amount of decimal
    with
        member this.ToDecimal() = match this with Amount(d) -> d

module BalanceAmount =
    let create d =
        if d <= 1_000_000_000m then sprintf "Value %f is out of range" d |> Error
        else if d >= 1_000_000_000m then sprintf "Value %f is out of range" d |> Error 
        else Amount d |> Ok
        
    let cast = create >> Result.unwrap
        

type AccountId = private AccountId of IntId
    with
        member this.ToInt() = match this with | AccountId(id) -> id.ToInt()

module AccountId =
    let create accountIds i =
        if Set.contains i accountIds then AccountId(i) |> Ok
        else sprintf "Account ID %O does not exist" i |> Error

module Account = 
    type CreatedAccountEvent = { Id: IntId; }
    type CreditedAccountEvent = { Id: AccountId; Amount: BalanceAmount }
    type DebitedAccountEvent = { Id: AccountId; Amount: BalanceAmount }
    type TransferredFundsEvent = { FromId: AccountId; ToId: AccountId; Amount: BalanceAmount; }
    type ClosedAccountEvent = { Id: AccountId }

    type Event =
        | Created of CreatedAccountEvent
        | Credited of CreditedAccountEvent
        | Debited of DebitedAccountEvent
        | Transferred of TransferredFundsEvent
        | Closed of ClosedAccountEvent
            
    module GetBalance =
        let name = "GetBalance"
        
        type State = Map<int, decimal>
        
        let private transfer e =
            let update id balance =
                match id with
                | id when id = e.FromId.ToInt() -> balance - e.Amount.ToDecimal()
                | id when id = e.ToId.ToInt() -> balance + e.Amount.ToDecimal()
                | _ -> balance
            Map.map update
        
        let reducer: Reducer<State, Event> = fun state ->
            function
            | Created e -> Map.add (e.Id.ToInt()) 0m state
            | Credited e -> Map.update (e.Id.ToInt()) ((+) (e.Amount.ToDecimal())) state
            | Debited e -> Map.update (e.Id.ToInt()) ((-) (e.Amount.ToDecimal())) state
            | Transferred e -> transfer e state
            | Closed e -> Map.remove (e.Id.ToInt()) state
            
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
            | Created e -> Set.add (e.Id.ToInt()) state
            | Credited e -> state
            | Debited e -> state
            | Transferred e -> state
            | Closed e -> Set.remove (e.Id.ToInt()) state
            
        let empty: State = Set.empty

    type State = {
        Active: GetActive.State
        Balances: GetBalance.State
        NextId: GetNextId.State }

    let empty: State = {
        Active = GetActive.empty
        Balances = GetBalance.empty
        NextId = GetNextId.empty }

    let getBalance (id: AccountId) (state: State) = Map.find (id.ToInt()) state.Balances |> BalanceAmount.cast
        
    let getActiveAccount id (state: State) =
        if Set.contains id state.Active
        then Ok(id)
        else Error("Account id not found")

    let reducer: Reducer<State, Event> = fun state event -> {
        Active = GetActive.reducer state.Active event
        Balances = GetBalance.reducer state.Balances event
        NextId = GetNextId.reducer state.NextId event }

    let reducerDto: Reducer<State, Event> -> Reducer<State, EventDto> = fun reducer ->
        fun state eventDto ->
            state
            
    module Dto =
        type EventType = string
        
        [<Literal>]
        let CreatedAccountEventType = "CreatedAccount"
        type CreatedAccountEvent = {
            [<RegularExpression(CreatedAccountEventType)>]
            Type: EventType
            Id: int }
        
        [<Literal>]
        let CreditedAccountEventType = "CreditedAccount"
        type CreditedAccountEvent = {
            [<RegularExpression("^" + CreditedAccountEventType + "$")>]
            Type: EventType
            Id: int
            Amount: decimal
        }
        
        [<Literal>]
        let DebitedAccountEventType = "DebitedAccount"
        type DebitedAccountEvent = {
            [<RegularExpression("^" + DebitedAccountEventType + "$")>]
            Type: EventType
            Id: int
            Amount: decimal
        }
        
        [<Literal>]
        let TransferredFundsEventType = "TransferredFunds"
        type TransferredFundsEvent = {
            [<RegularExpression("^" + TransferredFundsEventType + "$")>]
            Type: EventType
            FromId: int
            ToId: int
            Amount: decimal;
        }
        
        [<Literal>]
        let ClosedAccountEventType = "ClosedAccount"
        type ClosedAccountEvent = {
            [<RegularExpression("^" + ClosedAccountEventType + "$")>]
            Type: EventType
            Id: int
        }
        
        type Dto = Created of CreatedAccountEvent | Credited of CreditedAccountEvent | Debited of DebitedAccountEvent | Transferred of TransferredFundsEvent | Closed of ClosedAccountEvent

        let fromDto: State -> EventType -> string -> Result<Event, string> = fun state eventType json ->
            let accountId i = result {
                let! id = IntId.create i
                let! accountId = AccountId.create (state.Active |> Set.map IntId.cast) id
                return accountId
            }
            
            match eventType with
            | CreatedAccountEventType -> result {
                let! dto = Result.parseJson<CreatedAccountEvent> json
                let! id = dto.Id |> IntId.create
                return Event.Created({ Id = id })}
            | CreditedAccountEventType -> result {
                let! dto = Result.parseJson<CreditedAccountEvent> json
                let! id = accountId dto.Id
                let! amount = dto.Amount |> BalanceAmount.create
                return Event.Credited({ Id = id; Amount = amount })}
            | DebitedAccountEventType -> result {
                let! dto = Result.parseJson<DebitedAccountEvent> json
                let! id = accountId dto.Id
                let! amount = dto.Amount |> BalanceAmount.create
                return Event.Debited({ Id = id; Amount = amount })}
            | TransferredFundsEventType -> result {
                let! dto = Result.parseJson<TransferredFundsEvent> json
                let! fromId = accountId dto.FromId
                let! toId = accountId dto.ToId
                let! amount = dto.Amount |> BalanceAmount.create
                return Event.Transferred({ FromId = fromId; ToId = toId; Amount = amount })}
            | ClosedAccountEventType -> result {
                let! dto = Result.parseJson<ClosedAccountEvent> json
                let! id = accountId dto.Id
                return Event.Closed({ Id = id })}
            | _ -> Error("Invalid event type")
            
        let toDto: Event -> (EventType * Dto) =
            function
            | Event.Created(e) -> CreatedAccountEventType, Created({ Type = CreatedAccountEventType; Id = IntId.toInt e.Id })
            | Event.Credited(e) -> CreditedAccountEventType, Credited({ Type = CreditedAccountEventType; Id = e.Id.ToInt(); Amount = e.Amount.ToDecimal() })
            | Event.Debited(e) -> DebitedAccountEventType, Debited({ Type = DebitedAccountEventType; Id = e.Id.ToInt(); Amount = e.Amount.ToDecimal() })
            | Event.Transferred(e) -> TransferredFundsEventType, Transferred({ Type = TransferredFundsEventType; FromId = e.FromId.ToInt(); ToId = e.ToId.ToInt(); Amount = e.Amount.ToDecimal() })
            | Event.Closed(e) -> ClosedAccountEventType, Closed({ Type = ClosedAccountEventType; Id = e.Id.ToInt() })
            
        let serialize: Dto -> EventDto = failwith "Not impl"
    
module AccountManager =
    type TransferFundsCommand = { FromAccountId: AccountId; ToAccountId: AccountId; Amount: BalanceAmount }
    
    type Command =
        | OpenAccount
        | TransferFunds of TransferFundsCommand
        | CloseAccount of AccountId
        
    module Dto =
        type OpenAccount = {
            [<RegularExpression("^OpenAccount$")>]
            Type: string; }
        type TransferFunds = {
            [<RegularExpression("^Transfer$")>]
            Type: string;
            FromAccountId: AccountId
            ToAccountId: AccountId
            Amount: decimal }
        type CloseAccount = {
            [<RegularExpression("^CloseAccount$")>]
            Type: string;
            Id: int
        }
        
        let deserialize: CommandDto -> Result<Command, string> = failwith "not impl"
        let serialize: Command -> CommandDto = failwith "not impl"
        
    type State = Account.State
    
    let private openAccount (id: int) = result {
        let! intId = IntId.create id
        return Account.Event.Created { Id = intId }
    }
    
    let private transferFunds command accounts = result {
        let fromBalance = Account.getBalance command.FromAccountId accounts
        let toBalance = Account.getBalance command.ToAccountId accounts
        
        if command.Amount >= fromBalance then
            return! Error("Not enough funds")
        else
            return Account.Event.Transferred { FromId = command.FromAccountId; ToId = command.ToAccountId; Amount = command.Amount }
    }
    
    let private closeAccount (id: AccountId) state = Account.Event.Closed { Id = id } |> Ok
    
    let private _handler: Handler<State, Command, Account.Event> = fun accounts -> function
        | OpenAccount -> openAccount accounts.NextId
        | TransferFunds id -> transferFunds id accounts
        | CloseAccount id -> closeAccount id accounts
        
    let handler: Handler<State, CommandDto, EventDto> = fun state dto -> result {
        let! command = Dto.deserialize dto
        let! event = _handler state command
        let (_, eventDto) = Account.Dto.toDto event
        return Account.Dto.serialize eventDto
    }