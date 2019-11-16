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

open System

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
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let shouldfindAccruedHolidaysToDaysWhenIsEmpty =
  testList "tests findAccruedHolidaysToDays" [
    test "A request to findAccruedHolidaysToDays when is empty" {
      let userRequestsState = Map.empty
      let userId = "jdoe"
      let dateConsultation = DateTime.Now

      Expect.isTrue (Logic.findAccruedHolidaysToDays userRequestsState userId dateConsultation = 0) "Requests should findAccruedHolidaysToDays"
    }
  ]

[<Tests>]
let shouldfindAccruedHolidaysToDaysWhenIsOutOfBounds =
  testList "tests findAccruedHolidaysToDays" [
    test "A request to findAccruedHolidaysToDays when is out of bounds" {
      // Given
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }
      let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
      let userId = "jdoe"
      let dateConsultation = DateTime.Now

      // Then
      Expect.isTrue (Logic.findAccruedHolidaysToDays userRequestsState userId dateConsultation = 0)
          "Requests should findAccruedHolidaysToDays"
    }
  ]

[<Tests>]
let shouldfindAccruedHolidaysToDaysWhenIsNotEmpty =
  testList "tests findAccruedHolidaysToDays" [
    test "A request to findAccruedHolidaysToDays when is not empty" {
      // Given
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 27); HalfDay = PM } }
      let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
      let userId = "jdoe"
      let dateConsultation = DateTime.Now

      // Then
      Expect.isTrue (Logic.findAccruedHolidaysToDays userRequestsState userId dateConsultation <> 0)
          "Requests should findAccruedHolidaysToDays"
    }
  ]

[<Tests>]
let shouldfindRemainingHolidaysFromLastYearWhenIsNotEmpty =
  testList "tests findRemainingHolidaysFromLastYear" [
    test "A request to findRemainingHolidaysFromLastYear when is not empty" {
      // Given
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 27); HalfDay = PM } }
      let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
      let userId = "jdoe"
      let dateConsultation = DateTime.Now

      // Then
      Expect.isTrue (Logic.findRemainingHolidaysFromLastYear userRequestsState userId dateConsultation <> 0)
          "Requests should findRemainingHolidaysFromLastYear"
    }
  ]

[<Tests>]
let shouldfindRemainingHolidaysFromLastYearWhenIsOutOfBounds =
  testList "tests findRemainingHolidaysFromLastYear" [
    test "A request to findRemainingHolidaysFromLastYear when is out of bounds" {
      // Given
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }
      let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
      let userId = "jdoe"
      let dateConsultation = DateTime.Now

      // Then
      Expect.isTrue (Logic.findAccruedHolidaysToDays userRequestsState userId dateConsultation = 0)
          "Requests should findRemainingHolidaysFromLastYear"
    }
  ]

[<Tests>]
let shouldFindActiveRequestsWhenIsEmpty =
  testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is empty" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2018, 12, 27); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.Validated request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId dateConsultation = 0)
            "Requests should findActiveRequests"
      }
  ]

[<Tests>]
let shouldFindActiveRequestsWhenIsNotEmpty =
    testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is not empty" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 27); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 28); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId dateConsultation <> 0)
            "Requests should findActiveRequests"
      }
    ]

[<Tests>]
let shouldFindActiveRequestsWhenIsOutOfBounds =
    testList "tests findActiveRequests" [
      test "A request to findActiveRequests when is out of bounds" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2021, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2021, 12, 28); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findActiveRequests userRequestsState userId dateConsultation = 0)
            "Requests should findActiveRequests"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsNotEmpty =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is not empty" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId dateConsultation <> 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsEmpty =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is empty" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 27); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 28); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId dateConsultation = 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindFutureHolidaysWhenIsLimit =
    testList "tests findFutureHolidays" [
      test "A request to findFutureHolidays when is limit" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 11, 16); HalfDay = AM }
          End = { Date = DateTime(2019, 11, 16); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now

        // Then
        Expect.isTrue (Logic.findFutureHolidays userRequestsState userId dateConsultation = 0)
            "Requests should findFutureHolidays"
      }
    ]

[<Tests>]
let shouldfindAvailableHolidaysWhenIsNotEmpty =
    testList "tests findAvailableHolidays" [
      test "A request to findAvailableHolidays when is not empty" {
        // Given
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }
        let userRequestsState = Map.ofList [ (Guid.NewGuid(), Logic.PendingValidation request) ]
        let userId = "jdoe"
        let dateConsultation = DateTime.Now
        let user = Employee "jdoe"

        // Then
        Expect.isTrue (Logic.findAvailableHolidays userRequestsState user dateConsultation <> 0)
            "Requests should findAvailableHolidays"
      }
 ]

[<Tests>]
let shouldfindAvailableHolidaysWhenIsEmpty =
    testList "tests findAvailableHolidays" [
      test "A request to findAvailableHolidays when is empty" {
        // Given
        let userRequestsState = Map.empty
        let userId = "jdoe"
        let dateConsultation = DateTime.Now
        let user = Employee "jdoe"

        // Then
        Expect.isTrue (Logic.findAvailableHolidays userRequestsState user dateConsultation = 0)
            "Requests should findAvailableHolidays"
      }
   ]