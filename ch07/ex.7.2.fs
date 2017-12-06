module RealTimeQueue

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Queue<'a> = Stream<'a> * 'a list * Stream<'a>

let empty<'a> : Queue<'a> = (lazy Nil, [], lazy Nil)

let isEmpty (q : Queue<'a>) : bool =
    match q with
    | (Lazy Nil, _, _) -> true
    | _ -> false

let rec rotate (q : Queue<'a>) : Stream<'a> =
    match q with
    | (Lazy Nil, y :: _, a) -> lazy (Cons (y, a))
    | (Lazy (Cons (x, xs)), y :: ys, a) ->
        lazy (Cons (x, rotate (xs, ys, lazy (Cons (y, a)))))

let exec (q : Queue<'a>) : Queue<'a> =
    match q with
    | (f, r, Lazy (Cons (_, s))) -> (f, r, s)
    | (_, _, Lazy Nil) -> let f' = rotate q in (f', [], f')

let snoc ((f, r, s) : Queue<'a>, x : 'a) : Queue<'a> =
    exec (f, x :: r, s)

let head (q : Queue<'a>) : 'a =
    match q with
    | (Lazy Nil, _, _) -> failwith "empty queue"
    | (Lazy (Cons (x, _)), _, _) -> x

let tail (q : Queue<'a>) : Queue<'a> =
    match q with
    | (Lazy Nil, _, _) -> failwith "empty queue"
    | (Lazy (Cons (_, f)), r, s) -> exec (f, r, s)

let one = snoc (empty, 1)
// exec ({}, [1], {})
// (rotate ({}, [1], {}), [], rotate ({}, [1], {}))
// ({1}, [], {1})
printfn "%A" one
// => (値は作成されていません。, [], 値は作成されていません。)
let onetwo = snoc (one, 2)
// exec ({1}, [2], {1})
// (1 : {}, [2], {})
printfn "%A" onetwo
// => (FSI_0060+StreamCell`1+Cons[System.Int32], [2], 値は作成されていません。)
let two = tail onetwo
// exec ({}, [2], {})
// (rotate ({}, [2], {}), [], rotate ({}, [2], {}))
// ({2}, [], {2})
printfn "%A" two
// => (値は作成されていません。, [], 値は作成されていません。)
printfn "%A" (head two)
// head (2 : {}, [], 2 : {})
// => 2
printfn "%A" two
// => (FSI_0103+StreamCell`1+Cons[System.Int32], [], FSI_0103+StreamCell`1+Cons[System.Int32])
