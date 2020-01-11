module QueryHandler

open Microsoft.AspNetCore.Http
open TimeOff
open System
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open Storage.Events

[<CLIMutable>]
type UserAndRequestId = {
    UserId: UserId
    RequestId: Guid
    Date: DateTime
}
    
[<CLIMutable>]
type UserAndDate = {
    UserId: UserId
    Date: DateTime
}

let handleQuery (eventStore: IStore<UserId, RequestEvent>) (user: User) (command: Command) =
    let userId = command.UserId

    let eventStream = eventStore.GetStream(userId)
    let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

    // Decide how to handle the command
    let result = Logic.query state user command

    // Finally, return the result
    result

let history (eventStore: IStore<UserId, RequestEvent>) = 
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let eventStream = eventStore.GetStream(userAndRequestId.UserId)
            let allRequestEvent = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty
            let currentDate = userAndRequestId.Date
            let sortedRequests = 
                allRequestEvent
                |> Map.toSeq 
                |> Seq.sortBy (fun (_, state) -> state.Request.Start.Date)
                |> Seq.filter (fun (_, state) -> state.Request.Start.Date <= currentDate)

            let result = if Seq.isEmpty sortedRequests then Error "Empty list of request" else Ok sortedRequests
                
            match result with
            | Ok requests -> return! json requests next ctx
            | Error message -> return! (BAD_REQUEST message) next ctx
        }

let getTimeOffInfo (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndDate = ctx.BindQueryString<UserAndDate>()
            let command = QueryRequest (userAndDate.UserId, userAndDate.Date)
            let eventStream = eventStore.GetStream(command.UserId)
            let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

            // Decide how to handle the command
            let result = Logic.query state user command

            match result with
            | Ok [TimeOffInfoRequested timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }