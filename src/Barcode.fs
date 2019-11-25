module Barcode

open Digits
open Number.Types
open Number.Values
open Control
open Control.Function

let private join a b c = Digits.ToString a + "-" + Digits.ToString b + "-" + Digits.ToString c

type NDC_11 = NDC_11 of Digits<_5> * Digits<_4> * Digits<_2>

type NDC_10 =
    | NDC_4_4_2 of Digits<_4> * Digits<_4> * Digits<_2>
    | NDC_5_3_2 of Digits<_5> * Digits<_3> * Digits<_2>
    | NDC_5_4_1 of Digits<_5> * Digits<_4> * Digits<_1> with
    static member AsString self =
        match self with
        | NDC_4_4_2(a, b, c) -> join a b c
        | NDC_5_3_2(a, b, c) -> join a b c
        | NDC_5_4_1(a, b, c) -> join a b c

module NDC11 =
    let private Return = curry3 NDC_11
    let ToString (NDC_11 (a, b, c)) = join a b c
    let FromNDC10 = function
    | NDC_4_4_2(x, y, z) -> let x = Digits.create ("0" + Digits.ToString x) _5 in Option.lift1 (skip1 Return __ y z) x
    | NDC_5_3_2(x, y, z) -> let y = Digits.create ("0" + Digits.ToString y) _4 in Option.lift1 (skip2 Return x __ z) y
    | NDC_5_4_1(x, y, z) -> let z = Digits.create ("0" + Digits.ToString z) _2 in Option.lift1 (skip3 Return x y __) z

let possible (s: Digits<_10>) =
    let s = Digits.ToString s
    let x = 
        let x = Digits.create s.[0..3] _4
        let y = Digits.create s.[4..7] _4
        let z = Digits.create s.[8..9] _2
        Option.liftT3 NDC_4_4_2 x y z
    let y =
        let x = Digits.create s.[0..4] _5
        let y = Digits.create s.[5..7] _3
        let z = Digits.create s.[8..9] _2
        Option.liftT3 NDC_5_3_2 x y z
    let z =
        let x = Digits.create s.[0..4] _5
        let y = Digits.create s.[5..8] _4
        let z = Digits.create s.[9..] _1
        Option.liftT3 NDC_5_4_1 x y z
    Option.lift3 (fun x y z -> [| x; y; z |]) x y z

module Regex =
    open FSharp.Text.RegexProvider

    type NDC442Regex = Regex< @"(?<A>\d{4})-(?<B>\d{4})-(?<C>\d{2})">
    type NDC532Regex = Regex< @"(?<A>\d{5})-(?<B>\d{3})-(?<C>\d{2})">
    type NDC541Regex = Regex< @"(?<A>\d{5})-(?<B>\d{4})-(?<C>\d{1})">

    let tryNDC442 s =
        NDC442Regex().TryTypedMatch(s) |> Option.bind (fun r ->
            let x = digits r.A.Value _4
            let y = digits r.B.Value _4
            let z = digits r.C.Value _2
            Option.liftT3 NDC_4_4_2 x y z
        )

    let tryNDC532 s = fun () ->
        NDC532Regex().TryTypedMatch(s) |> Option.bind (fun r ->
            let x = digits r.A.Value _5
            let y = digits r.B.Value _3
            let z = digits r.C.Value _2
            Option.liftT3 NDC_5_3_2 x y z
        )

    let tryNDC541 s = fun () ->
        NDC541Regex().TryTypedMatch(s) |> Option.bind (fun r ->
            let x = digits r.A.Value _5
            let y = digits r.B.Value _4
            let z = digits r.C.Value _1
            Option.liftT3 NDC_5_4_1 x y z
        )

    let tryParse s = tryNDC442 s |> Option.orElseWith (tryNDC532 s) |> Option.orElseWith (tryNDC541 s)
