module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }

    [<CLIMutable>]
    type UserAndDate = {
        UserId: UserId
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

    let handleQuery (eventStore: IStore<UserId, RequestEvent>) (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.query state user command

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

    let getTimeOffInfo (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndDate = ctx.BindQueryString<UserAndDate>()
                let command = QueryRequest (userAndDate.UserId, userAndDate.Date)
                let result = handleQuery eventStore user command
                match result with
                | Ok [TimeOffInfoRequested timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx

            }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =
    
        
    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=> route "/request" >=> HttpHandlers.requestTimeOff (eventStore) (user)
                            POST >=> route "/validate-request" >=> HttpHandlers.validateRequest (eventStore) (user)
                            POST >=> route "/cancel-request" >=> HttpHandlers.cancelRequest (eventStore) (user)
                            POST >=> route "/submit-cancel-request" >=> HttpHandlers.submitCancelRequest (eventStore) (user)
                            POST >=> route "/reject-cancel-request" >=> HttpHandlers.rejectCancelRequest (eventStore) (user)

                            GET >=> route "/info" >=> HttpHandlers.getTimeOffInfo (eventStore) (user)
                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%s")

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0