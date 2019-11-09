namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest // Demander un congé
    | ValidateRequest of UserId * Guid // Valider une demande de congé
    | CancelRequest of UserId * Guid // Annuler une demande de congé
    | SubmitCancelRequest of UserId * Guid // Demander l'annulation d'une demande de congé
    | RejectCancelRequest of UserId * Guid // Annuler une demande d'annulatioin de demande de congé
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | SubmitCancelRequest (userId, _) -> userId
        | RejectCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | CancelRequestRejected of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
        | CancelRequestRejected request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | Canceled
        | PendingValidation of TimeOffRequest
        | PendingCancel of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Canceled -> invalidOp "Canceled"
            | PendingValidation request -> request
            | PendingCancel request -> request 
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Canceled -> false
            | PendingCancel _ -> true
            | PendingValidation _ -> true
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelled request -> Canceled
        | CancelRequestRejected request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        if request2.Start = request1.Start && request2.Start.HalfDay = request1.Start.HalfDay then
            true
        elif request2.End = request1.End && request2.End.HalfDay = request1.End.HalfDay then
            true
        // request2 > request1
        elif request2.Start > request1.Start && ( request2.Start > request1.End
            || request2.Start = request1.End && request1.End.HalfDay = HalfDay.AM && request2.Start.HalfDay = HalfDay.PM ) then
            false
        // request2 < request1
        elif request2.Start < request1.Start && ( request2.End < request1.Start
            || request2.End = request1.Start && request2.End.HalfDay = HalfDay.AM && request1.Start.HalfDay = HalfDay.PM ) then
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
            | CancelRequest _ -> 
                Error "Not implemented" //  TODO
            | SubmitCancelRequest _ ->
                Error "Not implemented" // TODO
            | RejectCancelRequest _ ->
                Error " Not implemented"


    let findAccruedHolidaysToDays (userRequests: UserRequestsState) (userId: UserId) (consultationDate: DateTime) =
           let dateTmp = consultationDate.AddMonths(-1);
           let lastDayOfMonthConsultation = DateTime.DaysInMonth(dateTmp.Year, dateTmp.Month)
           let dateOfLastMonth = DateTime(dateTmp.Year , dateTmp.Month,  lastDayOfMonthConsultation)
           if Map.empty<Guid, RequestState> <> userRequests then
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.filter (fun state -> state.Request.Start.Date <= dateOfLastMonth)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.filter (fun state -> state.UserId = userId)
                    |> Seq.length
           else
               0

    let findRemainingHolidaysFromLastYear  (userRequests: UserRequestsState) (userId: UserId) (consultationDate: DateTime) =
        let dateLastYear = DateTime(consultationDate.Year - 1, 12,  31)
        if Map.empty<Guid, RequestState> <> userRequests then
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.filter (fun state -> state.Request.Start.Date <= dateLastYear)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.filter (fun state -> state.UserId = userId)
                    |> Seq.length
           else
               0


    let findActiveRequests (userRequests: UserRequestsState) (userId: UserId) (consultationDate: DateTime) =
           let beginYear = DateTime(consultationDate.Year , 1, 1)
           if Map.empty<Guid, RequestState> <> userRequests then
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.filter (fun state -> state.Request.Start.Date >= beginYear && state.Request.Start.Date <= consultationDate)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                    |> Seq.filter (fun state -> state.UserId = userId)
                    |> Seq.length
           else
               0


    let findFutureHolidays (userRequests: UserRequestsState) (userId: UserId) (consultationDate: DateTime)  =
           let tomorrow = consultationDate.AddDays(1.0)
           let endYear = DateTime(consultationDate.Year , 12, 31)
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


    let findAvailableHolidays (userRequests: UserRequestsState) (user: User) (consultationDate: DateTime) =
        match user with
        | Employee userId ->
            let accruedHolidaysToDays = findAccruedHolidaysToDays  userRequests userId consultationDate
            let remainingHolidaysFromLastYear = findRemainingHolidaysFromLastYear userRequests userId consultationDate
            let activeHolidays = findActiveRequests userRequests userId consultationDate
            let futureHolidays = findFutureHolidays userRequests userId consultationDate

            accruedHolidaysToDays + remainingHolidaysFromLastYear - (activeHolidays + futureHolidays)
        | _ -> invalidOp "User is not an employee"