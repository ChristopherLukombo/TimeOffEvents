module CommandHandler

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

let handleCommand (eventStore: IStore<UserId, RequestEvent>) (user: User) (command: Command) =
    let userId = command.UserId

    let eventStream = eventStore.GetStream(userId)
    let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

    // Decide how to handle the command
    let result = Logic.decide state user command

    // Save events in case of success
    match result with
    | Ok events -> eventStream.Append(events)
    | _ -> ()

    // Finally, return the result
    result

let requestTimeOff (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
            let command = RequestTimeOff timeOffRequest
            let result = handleCommand eventStore user command
            match result with
            | Ok _ -> return! json timeOffRequest next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }

let validateRequest (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
            let result = handleCommand eventStore user command
            match result with
            | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }

let cancelRequest (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
            let result = handleCommand eventStore user command
            match result with
            | Ok [RequestCancelled timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }

let submitCancelRequest (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = SubmitCancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
            let result = handleCommand eventStore user command
            match result with
            | Ok [CancelRequestSubmitted timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }

let rejectCancelRequest (eventStore: IStore<UserId, RequestEvent>) (user: User) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = RejectCancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
            let result = handleCommand eventStore user command
            match result with
            | Ok [CancelRequestRejected timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
        }