module PhysicistsQueue

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Queue<'a> = int * Stream<'a> * int * Stream<'a>

let empty<'a> : Queue<'a> = (0, lazy Nil, 0, lazy Nil)

let isEmpty ((lenf, _, _, _) : Queue<'a>) : bool = lenf = 0

let rec rotate (q : Stream<'a> * Stream<'a> * Stream<'a>) : Stream<'a> =
    match q with
    | (Lazy Nil, Lazy (Cons (y, _)), a) -> lazy (Cons (y, a))
    | (Lazy (Cons (x, xs)), Lazy (Cons (y, ys)), a) ->
        lazy (Cons (x, rotate (xs, ys, lazy (Cons (y, a)))))

let check ((lenf, f, lenr, r) as q : Queue<'a>) : Queue<'a> =
    if lenr <= lenf then q else (lenf + lenr, rotate (f, r, lazy Nil), 0, lazy Nil)

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

let one : Queue<int> = snoc (empty, 1)
// check (0, {}, 1, {1})
// (1, rotate ({}, {1}, {}), 0, {})
// (1, {1}, 0, {})
printfn "%A" one
// => (1, 値は作成されていません。, 0, 値は作成されていません。)
let onetwo : Queue<int> = snoc (one, 2)
// check (1, {1}, 1, {2})
// (1, {1}, 1, {2})
printfn "%A" onetwo
// => (1, 値は作成されていません。, 1, 値は作成されていません。)
printfn "%A" (head onetwo)
// head (1, 1 : {}, 1, {2})
// => 1
printfn "%A" onetwo
// => (1, FSI_0079+StreamCell`1+Cons[System.Int32], 1, 値は作成されていません。)
let two = tail onetwo
// (1, rotate ({}, {2}, {}), 0, {})
// (1, {2}, 0, {})
printfn "%A" two
// => (1, 値は作成されていません。, 0, 値は作成されていません。)
printfn "%A" onetwo
// (1, FSI_0117+StreamCell`1+Cons[System.Int32], 1, FSI_0117+StreamCell`1+Cons[System.Int32])
printfn "%A" (head two)
// => 2
printfn "%A" two
// => (1, FSI_0079+StreamCell`1+Cons[System.Int32], 0, 値は作成されていません。)
