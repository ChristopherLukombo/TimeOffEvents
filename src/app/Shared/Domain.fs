namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM // AM < PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

[<CLIMutable>]
type TimeOffInfo = {
    UserId: UserId
    AccruedToDate: int
    CarriedOver: int
    TakenToDate: int
    Planned: int
    CurrentBalance: int
}

type IDateProvider =
  abstract member CurrentDate : DateTime
  abstract member DateTime : int -> int -> int -> DateTime

type DateProvider() =
    interface IDateProvider with
        //member this.CurrentDate = DateTime.Now
        member this.CurrentDate = DateTime(2020, 01, 10)
        member this.DateTime year month day = DateTime(year, month, day)