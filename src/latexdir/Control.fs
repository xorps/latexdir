module Control

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

