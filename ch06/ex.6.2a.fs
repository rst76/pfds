module BankersQueue

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Queue<'a> = int * Stream<'a> * int * Stream<'a>

let rec (++) (xs : Stream<'a>) (ys : Stream<'a>) : Stream<'a> =
    match xs with
    | Lazy Nil -> ys
    | Lazy (Cons (x, xs')) -> lazy (Cons (x, xs' ++ ys))

let reverse (xs : Stream<'a>) : Stream<'a> =
    let rec reverse' xs ys =
        match xs with
        | Lazy Nil -> ys
        | Lazy (Cons (x, xs')) -> reverse' xs' (lazy (Cons (x, ys)))
    reverse' xs (lazy Nil)

let empty<'a> : Queue<'a> = (0, lazy Nil, 0, lazy Nil)

let check ((lenf, f, lenr, r) as q : Queue<'a>) : Queue<'a> =
    if lenr <= lenf then q
    else (lenf + lenr, f ++ reverse r, 0, lazy Nil)

let snoc ((lenf, f, lenr, r) : Queue<'a>, x : 'a) =
    check (lenf, f, lenr + 1, lazy (Cons (x, r)))

let head (q : Queue<'a>) : 'a =
    match q with
    | (_, Lazy Nil, _, _) -> failwith "empty queue"
    | (_, Lazy (Cons (x, _)), _, _) -> x

let tail (q : Queue<'a>) : Queue<'a> =
    match q with
    | (_, Lazy Nil, _, _) -> failwith "empty queue"
    | (lenf, Lazy (Cons (_, f)), lenr, r) -> check (lenf - 1, f, lenr, r)
