module Control

module Object =
    let ToString = fun it -> it.ToString()

module Function =
    type Skip() = class end
    let __ = Skip()
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

module Task =
    open FSharp.Control.Tasks.V2.ContextInsensitive
    let from a = task { return a }
    let map<'a, 'b> (f : 'a -> 'b) (t : System.Threading.Tasks.Task<'a>) = task {
        let! res = t
        return f res
    }   
    let bind<'a, 'b> (f : 'a -> System.Threading.Tasks.Task<'b>) (t : System.Threading.Tasks.Task<'a>) = task {
        let! res = t
        return! f res
    }

module TaskOption =
    let map f = Task.map (Option.map f)

module OptionArray =
    let map f = Option.map (Array.map f)

module TaskResult =
    type TaskResultBuilder() =
        member _.Bind<'a, 'b, 'e>(x, f) = x |> Task.bind<Result<'a, 'e>, Result<'b, 'e>> (fun res ->
            match res with
            | Ok a -> f a
            | Error e -> Task.from (Error e)
        )
        member _.Return(x) = Task.from (Ok x)
        member _.Zero() = Task.from (Ok ())

    let taskresult = TaskResultBuilder()

    let from x = Task.from (Ok x)

