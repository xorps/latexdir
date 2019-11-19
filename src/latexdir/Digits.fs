module Digits

type Digits<'T when 'T :> Number.Number> = private Digits of string

module Digits =
    let create (s: string) (n: 'T when 'T :> Number.Number) =
        if s.Length = n.Int && String.forall System.Char.IsDigit s then
            Some (Digits s)
        else 
            None
    let ToString (Digits s) = s
