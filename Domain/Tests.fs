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

[<Fact>]
let ``Nuget`` () =
    let generator = JSchemaGenerator()
    let schema = generator.Generate(typeof<Account.Dto.ClosedAccountEvent>)
    schema |> should equal ""
    