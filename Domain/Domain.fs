module Domain

open Shared

type State = {
    Head: int
    Accounts: Accounts.Account.State }

module State = 
    type Reducer = Shared.Reducer<State, EventDto>
        
    let empty: State = {
        Head = 0
        Accounts = Accounts.Account.State.empty }
    
    let reducer: Reducer = fun state dto ->
        if dto.Id <> state.Head + 1
        then failwithf "Event Id %i is not sequential to head %i" dto.Id state.Head
        else
            { state with
                Head = state.Head + 1
                Accounts = Accounts.Account.reducer (state.Accounts) dto }
        
module Commands =
    type CommandId = string
    
    type Handler = Shared.Handler<State, CommandDto, EventDto> 
    
    let handlers: Handler list = [
        Handler.mapState Accounts.AccountManager.handler (fun s -> (s.Accounts, s.Head + 1))
    ]
    
    let handler: Handler = Handler.tryPick handlers

let processCommand state command =
    match Commands.handler state command with
    | Ok(event) ->(State.reducer state event, event) |> Ok
    | Error(e) -> Error(e)