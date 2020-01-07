module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [

    let startRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

    yield test "A request overlaps with itself" {
      Expect.isTrue (Logic.overlapsWith startRequest startRequest) "A request should overlap with istself"
    }

    yield test "Requests on 2 distinct days don't overlap" {
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith startRequest request2) "The requests don't overlap"
    }

    yield test "Requests overlap when being on same HalfDay" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "Requests should overlap"
    }

    yield test "Requests overlap when being on same HalfDay reverse" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request2 request1) "Requests should overlap"
    }

    yield test "Requests don't overlap when being on different HalfDay" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = AM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "Requests should not overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }


    yield test "A request is created" {
      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [

    let request = {
      UserId = "jdoe"
      RequestId = Guid.NewGuid()
      Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
      End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

    yield test "A request is validated" {

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    yield test "Wrong user cannot validate a request" {

      let userEmployee = 
        Employee "3ae70716-0be7-11ea-8d71-362b9e155667"

      Given [ RequestCreated request]
      |> ConnectedAs userEmployee
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request shouldn't be validated"
    }
  ]

[<Tests>]
let shouldfindAccruedHolidaysToDaysWhenIsNotEmpty =
  testList "tests findAccruedHolidaysToDays" [
    test "A request to findAccruedHolidaysToDays when is not empty" {
      // Given
      let dateProvider = DateProvider()
      let consultationDate = (dateProvider :> IDateProvider).DateTime 2019 3 28

      // Then
      Expect.isTrue (Logic.findAccruedHolidaysToDays consultationDate = 4)
          "Requests should findAccruedHolidaysToDays"
    }
  ]

[<Tests>]
let cancellingRequestTest = 
  testList "Cancel Request tests" [

    let dateProvider = DateProvider()
    let yesterday = (dateProvider :> IDateProvider).CurrentDate.AddDays(-1.0) // Yesterday
    let tomorrow = (dateProvider :> IDateProvider).CurrentDate.AddDays(1.0) // Tomorrow
    let oneWeekInTheFuture = (dateProvider :> IDateProvider).CurrentDate.AddDays(7.0) // In one week

    let request = {
      UserId = "jdoe"
      RequestId = Guid.NewGuid()
      Start = { Date = tomorrow ; HalfDay = AM }
      End = { Date = oneWeekInTheFuture; HalfDay = PM }
    }

    let pendingCancelRequest = {
      UserId = "jdoe"
      RequestId = Guid.NewGuid()
      Start = { Date = yesterday ; HalfDay = AM }
      End = { Date = oneWeekInTheFuture; HalfDay = PM }
    }

    yield test "Cancel PendingValidation request succeed" {
      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When ( CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should be cancelled"
    }

    yield test "Cancel Validated request succeed" {
      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should be cancelled"
    }

    yield test "Cancel PendingCancel request succeed" {
      Given [ CancelRequestSubmitted pendingCancelRequest ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", pendingCancelRequest.RequestId))
      |> Then (Ok [ RequestCancelled pendingCancelRequest ]) "The request should be cancelled"
    }
  ]

let shouldfindAccruedHolidaysToDaysWhenIEmptyForFirstMonth =
  testList "tests findAccruedHolidaysToDays" [
    test "A request to findAccruedHolidaysToDays when is empty for first month" {
      // Given
      let dateProvider = DateProvider()
      let consultationDate = (dateProvider :> IDateProvider).DateTime 2019 1 28

      // Then
      Expect.isTrue (Logic.findAccruedHolidaysToDays consultationDate = 0)
          "Requests should findAccruedHolidaysToDays for first day"
    }
  ]

[<Tests>]
let shouldfindRemainingHolidaysFromLastYearWhenIsNotEmpty =
  testList "tests findRemainingHolidaysFromLastYear" [
    test "A request to findRemainingHolidaysFromLastYear when is not empty" {
      // Given
      let dateProvider = DateProvider()
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = (dateProvider :> IDateProvider).DateTime 2018 12 27; HalfDay = AM }
        End = { Date = (dateProvider :> IDateProvider).DateTime 2018 12 27; HalfDay = PM } }
      let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
      let userId = "jdoe"
      let consultationDate = (dateProvider :> IDateProvider).DateTime 2019 1 1

      // Then
      Expect.isTrue (Logic.findRemainingHolidaysFromLastYear userRequestsState userId consultationDate = 39)
          "Requests should findRemainingHolidaysFromLastYear"
    }
  ]


[<Tests>]
let shouldFindActiveRequestsWhenIsEmpty =
  testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is empty" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date =  (dateProvider :> IDateProvider).DateTime 2018 12 27; HalfDay = AM }
          End = { Date = (dateProvider :> IDateProvider).DateTime 2018 12 27; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).DateTime 2019 1 1

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId consultationDate = 0)
            "Requests should findActiveRequests"
      }
  ]

[<Tests>]
let shouldFindActiveRequestsWhenIsNotEmpty =
    testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is not empty" {
        // Given
        let dateProvider = DateProvider()
        
        let yesterday = (dateProvider :> IDateProvider).CurrentDate.AddDays(-1.0) // Yesterday
        let twoDaysAgo = (dateProvider :> IDateProvider).CurrentDate.AddDays(-2.0) // twoDaysAgo
        
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = twoDaysAgo; HalfDay = AM }
          End = { Date = yesterday; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (request.RequestId, Logic.PendingValidation request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).CurrentDate

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId consultationDate <> 0)
            "Requests should findActiveRequests"
      }
    ]

[<Tests>]
let shouldFindActiveRequestsWhenIsOutOfBounds =
    testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is out of bounds" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = (dateProvider :> IDateProvider).DateTime 2021 12 27; HalfDay = AM }
          End = { Date = (dateProvider :> IDateProvider).DateTime 2021 12 28; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).CurrentDate

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId consultationDate = 0)
            "Requests should findActiveRequests"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsNotEmpty =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is not empty" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = (dateProvider :> IDateProvider).DateTime 2020 12 27; HalfDay = AM }
          End = { Date =  (dateProvider :> IDateProvider).DateTime 2020 12 28; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).DateTime 2020 5 6

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId consultationDate <> 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsEmpty =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is empty" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = (dateProvider :> IDateProvider).DateTime 2019 5 10; HalfDay = AM }
          End = { Date = (dateProvider :> IDateProvider).DateTime 2019 5 28; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).DateTime 2020 5 6

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId consultationDate = 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsLimit =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is limit" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = (dateProvider :> IDateProvider).DateTime 2019 11 16; HalfDay = AM }
          End = { Date = (dateProvider :> IDateProvider).DateTime 2019 11 16; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let consultationDate = (dateProvider :> IDateProvider).CurrentDate

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId consultationDate = 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindAvailableHolidaysWhenIsNotEmpty =
    testList "tests findAvailableHolidays" [
      test "A request to findAvailableHolidays when is not empty" {
        // Given
        let dateProvider = DateProvider()
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = (dateProvider :> IDateProvider).DateTime 2019 12 27; HalfDay = AM }
          End = { Date = (dateProvider :> IDateProvider).DateTime 2020 12 28; HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let consultationDate = (dateProvider :> IDateProvider).CurrentDate
        let user = Employee "jdoe"

        // Then
        Expect.isTrue (Logic.findAvailableHolidays userRequestsState user consultationDate <> 0)
            "Requests should findAvailableHolidays"
      }
 ]

[<Tests>]
let shouldfindAvailableHolidaysWhenIsEmpty =
    testList "tests findAvailableHolidays" [
      test "A request to findAvailableHolidays when is empty taken" {
        // Given
        let dateProvider = DateProvider()
        let userRequestsState = Map.empty
        let consultationDate = (dateProvider :> IDateProvider).DateTime 2019 3 28
        let user = Employee "jdoe"

        // Then
        Expect.isTrue (Logic.findAvailableHolidays userRequestsState user consultationDate = 4)
            "Requests should findAvailableHolidays"
      }
   ]
