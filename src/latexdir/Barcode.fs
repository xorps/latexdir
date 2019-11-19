module Barcode

open Digits
open Number.Types
open Number.Values
open Control
open Control.Function

type NDC_11 = NDC_11 of Digits<_5> * Digits<_4> * Digits<_2>

let Mk_NDC_11 = curry3 NDC_11

type NDC_10 =
    | NDC_4_4_2 of Digits<_4> * Digits<_4> * Digits<_2>
    | NDC_5_3_2 of Digits<_5> * Digits<_3> * Digits<_2>
    | NDC_5_4_1 of Digits<_5> * Digits<_4> * Digits<_1>

let to_NDC_11 = function
    | NDC_4_4_2(x, y, z) -> let x = Digits.create ("0" + Digits.ToString x) _5 in Option.lift1 (skip1 Mk_NDC_11 __ y z) x
    | NDC_5_3_2(x, y, z) -> let y = Digits.create ("0" + Digits.ToString y) _4 in Option.lift1 (skip2 Mk_NDC_11 x __ z) y
    | NDC_5_4_1(x, y, z) -> let z = Digits.create ("0" + Digits.ToString z) _2 in Option.lift1 (skip3 Mk_NDC_11 x y __) z

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
