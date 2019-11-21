module Builder

type OptionBuilder() =
    member _.Bind(x, f) = x |> Option.bind f
    member _.Return(x) = Some x

module Option =
    let option = OptionBuilder()
