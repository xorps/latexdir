module Control

module Object =
    let ToString = fun it -> it.ToString()

module Function =
    type Skip() = class end
    let __ = Skip()
    let uncurry2 f = fun (a, b) -> f a b
    let curry2 f a b = f (a, b)
    let curry3 f a b c = f (a, b, c)
    let skip1 f (_: Skip) b c = fun a -> f a b c
    let skip2 f a (_: Skip) c = fun b -> f a b c
    let skip3 f a b (_: Skip) = fun c -> f a b c

module Option =
    let lift1 f a =
        a |> Option.map (fun a -> f a)

    let lift3 f a b c =
        a |> Option.bind (fun x ->
        b |> Option.bind (fun y ->
        c |> Option.map (fun z ->
            f x y z
        )))
        
    let liftT3 f = lift3 (fun x y z -> f (x, y, z))

module Result =
    /// does not combine errors, fails with first error
    let zip<'a, 'b, 'e> ((a : Result<'a, 'e>), (b : Result<'b, 'e>)): Result<'a * 'b, 'e> =
        a |> Result.bind (fun x -> b |> Result.map (fun y -> x, y))
    /// somewhat like a comand
    let extend f = function
    | Ok a -> a
    | Error e -> f (Error e)

module Task =
    open FSharp.Control.Tasks.V2.ContextInsensitive
    open System.Threading.Tasks

    let from a = task { return a }

    let map<'a, 'b> (f : 'a -> 'b) (t : Task<'a>) = task {
        let! res = t
        return f res
    }

    let bind<'a, 'b> (f : 'a -> Task<'b>) (t : Task<'a>) = task {
        let! res = t
        return! f res
    }

    let zip<'a, 'b> (a : Task<'a>) (b : Task<'b>): Task<'a * 'b> = task {
        let! _ = Task.WhenAll(a, b)
        let! x = a
        let! y = b
        return x, y
    }

module TaskResult =
    open System.Threading.Tasks

    type TaskResultBuilder() =
        member _.Bind<'a, 'b, 'e>(x, f) = x |> Task.bind<Result<'a, 'e>, Result<'b, 'e>> (fun res ->
            match res with
            | Ok a -> f a
            | Error e -> Task.from (Error e)
        )
        member _.Return(x) = Task.from (Ok x)
        member _.Zero() = Task.from (Ok ())

    let from x = Task.from (Ok x)

    //let flatten = Task.bind (Result.extend Task.FromResult)

    let map<'a, 'b, 'e> (f: 'a -> 'b): Task<Result<'a, 'e>> -> Task<Result<'b, 'e>> = Task.map (Result.map f)

    let bind<'a, 'b, 'e> (f: 'a -> Task<Result<'b, 'e>>): Task<Result<'a, 'e>> -> Task<Result<'b, 'e>> =
        Task.bind (fun it -> it |> Result.map f |> Result.extend Task.FromResult)

    module Operators =
        let inline (<&>) m f = m |> map f
        let inline (>>=) m f = m |> bind f


module TaskResultBuilder =
    let taskresult = TaskResult.TaskResultBuilder()

