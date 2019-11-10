namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | Rejected
        | Canceled
        | PendingValidation of TimeOffRequest
        | PendingCancel of TimeOffRequest
        | CancelRejected of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Rejected -> invalidOp "Rejected"
            | Canceled -> invalidOp "Canceled"
            | PendingValidation request -> request
            | PendingCancel request -> request 
            | CancelRejected request -> request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Rejected -> false
            | Canceled -> false
            | PendingCancel _ -> true
            | PendingValidation _ -> true
            | CancelRejected _ -> true
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        if request1.Start < request2.Start && request1.End < request2.Start then
            false
        else if request2.Start < request1.Start && request2.End < request1.Start then
            false
        else
            true

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let overlapswithReq elem = overlapsWith request elem
        otherRequests
        |> Seq.map overlapswithReq
        |> Seq.contains true //TODO: write this function using overlapsWith

    let createRequest activeUserRequests  request =
        let a = Settings()
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= (a :> IDataProvider).Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState

    let findActiveRequests (userRequests: UserRequestsState) (userId: UserId) =
           let beginYear = DateTime(DateTime.Now.Year , 1, 1)
           let now = (Settings() :> IDataProvider).Today
           if Map.empty<Guid, RequestState> <> userRequests then
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.filter (fun state -> state.Request.Start.Date >= beginYear && state.Request.Start.Date <= now)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.filter (fun state -> state.UserId = userId)
                    |> Seq.length
           else
               0

    let findFutureHolidays (userRequests: UserRequestsState) (userId: UserId) =
           let tomorrow = (Settings() :> IDataProvider).Today.AddDays(1.1)
           let endYear = DateTime(DateTime.Now.Year , 1, 31)
           if Map.empty<Guid, RequestState> <> userRequests then
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.filter (fun state -> state.Request.Start.Date >= tomorrow && state.Request.Start.Date <= endYear)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.filter (fun state -> state.UserId = userId)
                    |> Seq.length
           else
               0

    let findAvailableHolidays (userRequests: UserRequestsState) (user: User) =
        match user with
        | Employee userId -> 
            let accruedHolidaysToDays = 0 // TODO : Calculer les jours de congé "gagné" depuis le début de l'année
            let remainingHolidaysFromLastYear = 0 // TODO : Récupérer les congés restant de l'année prédédente
            let activeHolidays = findActiveRequests userRequests userId
            let futureHolidays = findFutureHolidays userRequests userId

            accruedHolidaysToDays + remainingHolidaysFromLastYear - ( activeHolidays + futureHolidays)
        | _ -> invalidOp "User is not an employee"