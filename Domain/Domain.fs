module Domain

open System.Collections.Generic
open System.Threading.Tasks

open Shared
open Accounts

type State = {
    Accounts: Accounts.Account.State }

module State = 
    type Reducer = Shared.Reducer<State, EventDto>

    type EventSequenceId = int

    type StateSaveResult = {
        HeadId: EventSequenceId }

    type IStateRepository =
        abstract member Save: State -> Task<StateSaveResult>
        abstract member Read: EventSequenceId -> Task<IAsyncEnumerable<EventDto>>
        
module Commands =
    type CommandId = string
    
    type Handler = Shared.Handler<State, CommandDto, EventDto> 
    
    let handlers: Handler list = [
        Handler.mapState Accounts.AccountManager.handler (fun s -> s.Accounts)
    ]
    
    let handler: Handler = Handler.tryPick handlers
    
    type ICommandSource =
        abstract member Read: Task<IAsyncEnumerable<CommandDto>>
        abstract member Reply: CommandId -> Result<'a, string> -> unit 