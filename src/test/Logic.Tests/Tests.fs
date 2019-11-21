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
    let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }


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
let cancellingRequestTest = 
  testList "Cancel Request tests" [

    let request = {
      UserId = "jdoe"
      RequestId = Guid.NewGuid()
      Start = { Date = DateTime(2019,12, 23); HalfDay = AM }
      End = { Date = DateTime(2019, 12, 29); HalfDay = PM }
    }

    yield test "Cancel PendingValidation request succeed" {
      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should be cancelled"
    }

    yield test "Cancel Validated request succeed" {
      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should be cancelled"
    }

    yield test "Cancel PendingCancel request succeed" {
      Given [ CancelRequestSubmitted request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [ RequestCancelled request ]) "The request should be cancelled"
    }


  ]