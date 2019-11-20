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
    | CancelRequestSubmitted of TimeOffRequest
    | CancelRequestRejected of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
        | CancelRequestSubmitted request -> request
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
        | CancelRequestSubmitted request -> PendingCancel request
        | CancelRequestRejected request -> Validated request

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
        let dateProvider = DateProvider()
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= (dateProvider :> IDateProvider).CurrentDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | Validated request ->
            Ok [RequestCancelled request]
        | PendingCancel request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be canceled"
    
    let submitCancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [CancelRequestSubmitted request]
        | Validated request ->
            Ok [CancelRequestSubmitted request]
        | _ ->
            Error "Cannot request cancel for this request"

    let rejectCancelRequest requestState = 
        match requestState with
        | PendingCancel request -> 
            Ok [CancelRequestRejected request]
        | _ ->
            Error "The cancel request cannot be rejected"

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
            | CancelRequest (_, requestId) -> 
                if user <> Manager then 
                    Error "Unauthorized"
                else 
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated 
                    cancelRequest requestState
            | SubmitCancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                submitCancelRequest requestState
            | RejectCancelRequest (_, requestId) ->
                if user <> Manager then 
                    Error "Unauthorized"
                else 
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    rejectCancelRequest requestState


    let findAccruedHolidaysToDays (userRequests: UserRequestsState) (userId: UserId) (consultationDate: DateTime) =
           let dateTmp = consultationDate.AddMonths(-1);
           let lastDayOfMonthConsultation = DateTime.DaysInMonth(dateTmp.Year, dateTmp.Month)
           let dateProvider = DateProvider()
           let dateOfLastMonth = (dateProvider :> IDateProvider).DateTime dateTmp.Year dateTmp.Month lastDayOfMonthConsultation
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
        let dateProvider = DateProvider()
        let dateLastYear = (dateProvider :> IDateProvider).DateTime (consultationDate.Year - 1) 12 31
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
           let dateProvider = DateProvider()
           let beginYear = (dateProvider :> IDateProvider).DateTime consultationDate.Year 1 1
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
           let dateProvider = DateProvider()
           let endYear = (dateProvider :> IDateProvider).DateTime consultationDate.Year 12 31
           let tomorrow = consultationDate.AddDays(1.0)
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