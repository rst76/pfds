module BadPhysicistsQueue

type Queue<'a> = 'a list * int * Lazy<'a list> * int * 'a list

let empty<'a> : Queue<'a> = ([], 0, lazy [], 0, [])

let isEmpty ((_, lenf, _, _, _) : Queue<'a>) : bool = lenf = 0

let checkw (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], lenf, f, lenr, r) -> (f.Force(), lenf, f, lenr, r)
    | _ -> q

let check ((w, lenf, f, lenr, r) as q : Queue<'a>) : Queue<'a> =
    if lenr <= lenf then checkw q
    else checkw (w, lenf + lenr, lazy (f.Force() @ List.rev r), 0, [])

let snoc ((w, lenf, f, lenr, r) : Queue<'a>, x : 'a) =
    check (w, lenf, f, lenr + 1, x :: r)

let head (q : Queue<'a>) : 'a =
    match q with
    | ([], _, _, _, _) -> failwith "empty queue"
    | (x :: _, _, _, _, _) -> x

let tail (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], _, _, _, _) -> failwith "empty queue"
    | (_ :: w, lenf, f, lenr, r) -> check (w, lenf - 1, lazy (List.tail (f.Force())), lenr, r)

let q0 = Seq.fold (fun q x -> snoc (q, x)) empty {1 .. 14}
// => ([1], 7, 値は作成されていません。, 7, [14; 13; 12; 11; 10; 9; 8])
let q1 = snoc (q0, 1)
// => ([1], 15, 値は作成されていません。, 0, [])
let q2 = snoc (q0, 2)
// => ([1], 15, 値は作成されていません。, 0, [])
let q3 = snoc (q0, 3)
// => ([1], 15, 値は作成されていません。, 0, [])
tail q1
// => ([2; 3; .. 14; 1], 14, [2; 3; 4; ... ], 0, [])
tail q2
// => ([2; 3; .. 14; 2], 14, [2; 3; 4; ... ], 0, [])
tail q3
// => ([2; 3; .. 14; 3], 14, [2; 3; 4; ... ], 0, [])
