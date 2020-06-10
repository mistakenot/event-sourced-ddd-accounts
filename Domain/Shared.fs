module Shared

open System

type EventDto = {
    Id: int
    Type: string
    Body: string }

type CommandDto = {
    Id: string
    Type: string
    Body: string }

type Reducer<'State, 'Event> = 'State -> 'Event -> 'State

type Computed<'State, 'Derived> = 'State -> 'Derived

type HandlerResult<'Event> = Result<'Event, string>

type Handler<'State, 'Command, 'Event> = 'State -> 'Command -> HandlerResult<'Event>

module Handler =
    let mapState: Handler<'a, 'c, 'e> -> ('b -> 'a) -> Handler<'b, 'c, 'e> = fun handle map -> fun s c -> handle (map s) c
    
    let mapResult: Handler<'s, 'c, 'a> -> ('a -> 'b) -> Handler<'s, 'c, 'b> = fun handle map ->
        fun s c ->
            match handle s c with
            | Ok(v) -> map v |> Ok
            | Error(e) -> Error(e)
    
    let notHandlerMessage = "Handler does not handle this event"
    
    let notHandled: Result<'a, string> = Error (notHandlerMessage)
    
    let tryPick: Handler<'a, 'b, 'c> list -> Handler<'a, 'b, 'c> = fun handlers ->
        fun s c ->
            let handleAndPick handler = 
                match handler s c with
                | Ok(v) -> Some(Ok(v))
                | Error(e) when e = notHandlerMessage -> None
                | Error(e) -> Some(Error(e))
        
            match List.tryPick handleAndPick handlers with
            | Some(Ok(e)) -> Ok(e)
            | Some(Error(e)) -> Error(e)
            | None -> notHandled

let ofOption error = function Some s -> Ok s | None -> Error error

module Result =
    let parseInt (str: string) =
        match Int32.TryParse(str) with
        | true, i -> i |> Ok
        | _ -> sprintf "Failed to parse int from %s" str |> Error
        
    let parseDecimal (str: string) =
        match Decimal.TryParse(str) with
        | true, i -> i |> Ok
        | _ -> sprintf "Failed to parse int from %s" str |> Error
        
    let parseEnum<'T> str : 'T option = 
        match Enum.TryParse(typeof<'T>, str) with
        | true, e -> Some(downcast e)
        | _ -> None
        
    let parseJson<'T> (str: string) : Result<'T, string> =
        System.Text.Json.JsonSerializer.Deserialize<'T>(str) |> Ok
        
    let unwrap =
        function
        | Ok(r) -> r
        | Error(s) -> sprintf "Unwrapped result failed: %O" s |> failwith

    type Builder() =
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

let result = Result.Builder()

type ShortString = private ShortString of string

module ShortString =
    let create str =
        if String.IsNullOrEmpty(str) then Error("Value can not be null or empty")
        else if String.length str >= 100 then Error("Value must be less than 100 characters")
        else ShortString str |> Ok
    let toString (ShortString s) = s
    
    
type IntId = private IntId of int
    with
        member this.ToInt() = match this with | IntId(i) -> i
        member this.ToString() = this.ToInt().ToString()

module IntId =
    let create i =
        if i < 1 then "Id value must be a positive integer" |> Error
        else IntId i |> Ok
        
    let cast = create >> Result.unwrap
    
    let toInt (i: IntId) = i.ToInt()

type Json = private Json of string

module Json =
    let create str =
        if String.IsNullOrEmpty(str)
        then Error("Value can not be null or empty")
        else Json str |> Ok

module Map =
    let update: 'a -> ('b -> 'b) -> Map<'a, 'b> -> Map<'a, 'b> = fun key update map ->
        match Map.tryFind key map with
        | Some(value) -> Map.add key (update value) map
        | None -> map
        
    let findResult key = Map.tryFind key >> function | Some(v) -> Ok(v) | None -> Error (sprintf "Key %s not found" key)
        
let parseEnum<'T> s = Enum.TryParse