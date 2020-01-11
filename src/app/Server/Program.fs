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

module HttpHandlers =

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
                                POST >=> route "/request" >=> CommandHandler.requestTimeOff (eventStore) (user)
                                POST >=> route "/validate-request" >=> CommandHandler.validateRequest (eventStore) (user)
                                POST >=> route "/cancel-request" >=> CommandHandler.cancelRequest (eventStore) (user)
                                POST >=> route "/submit-cancel-request" >=> CommandHandler.submitCancelRequest (eventStore) (user)
                                POST >=> route "/reject-cancel-request" >=> CommandHandler.rejectCancelRequest (eventStore) (user)

                                GET >=> route "/history" >=> QueryHandler.history (eventStore)
                                GET >=> route "/info" >=> QueryHandler.getTimeOffInfo (eventStore) (user)
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