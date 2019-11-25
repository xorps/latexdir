module Digits

type Digits<'T when 'T :> Number.Number> = private Digits of string

module Digits =
    let create<'T when 'T :> Number.Number> (s: string) (n: 'T): Digits<'T> option =
        if s.Length = n.Int && String.forall System.Char.IsDigit s then
            Some (Digits s)
        else 
            None

    let ToString (Digits s) = s

let digits = Digits.create
