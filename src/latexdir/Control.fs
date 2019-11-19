module Control

module Option =
    let liftT3 f a b c =
        a |> Option.bind (fun x ->
        b |> Option.bind (fun y ->
        c |> Option.map (fun z ->
            f (x, y, z)
        )))

    let liftA3 a b c =
        a |> Option.bind (fun x ->
        b |> Option.bind (fun y ->
        c |> Option.map (fun z ->
            [| x; y; z |]
        )))
