module PhysicistsQueue

type Queue<'a> = 'a list * int * Lazy<'a list> * int * 'a list

let empty<'a> : Queue<'a> = ([], 0, lazy [], 0, [])

let isEmpty ((_, lenf, _, _, _) : Queue<'a>) : bool = lenf = 0

let checkw (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], lenf, f, lenr, r) -> (f.Force(), lenf, f, lenr, r)
    | _ -> q

let check ((w, lenf, f, lenr, r) as q : Queue<'a>) : Queue<'a> =
    if lenr <= lenf then checkw q
    else let f' = f.Force() in checkw (f', lenf + lenr, lazy (f' @ List.rev r), 0, [])

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

let q0 = empty
// => ([], 0, 値は作成されていません。, 0, [])
// P: 0
let q1 = snoc (q0, 1)
// => ([1], 1, [1], 0, [])
// uc: 2, sc: 0(uc: 1, sc: 1), P: 1
let q2 = snoc (q1, 2)
// => ([1], 1, [1], 1, [2])
// uc: 1, sc: 0, P: 0
let q3 = snoc (q2, 3)
// => ([1], 3, 値は作成されていません。, 0, [])
// uc: 1, sc: 3, P: 2
let q4 = snoc (q3, 4)
// => ([1], 3, 値は作成されていません。, 1, [4])
// uc: 1, sc: 0, P: 2
let q5 = snoc (q4, 5)
// => ([1], 3, 値は作成されていません。, 2, [5; 4])
// uc: 1, sc: 0, P: 1
let q6 = snoc (q5, 6)
// => ([1], 3, 値は作成されていません。, 3, [6; 5; 4])
// uc: 1, sc: 0, P: 0
let q5' = tail q6
// => ([2; 3], 5, 値は作成されていません。, 0, [])
// uc: 1, sc: 6, P: 4
let q7 = snoc (q6, 7)
// => ([1; 2; 3], 7, 値は作成されていません。, 0, [])
// uc: 1, sc: 7, P: 6
let q5'' = tail (tail q7)
// => ([3], 5, 値は作成されていません。, 0, [])
// uc: 2, sc: 2, P: 2
let q4' = tail q5''
// => ([4; 5; 6; 7], 4, [4; 5; 6; ... ], 0, [])
// uc: 6, sc: 0(uc:1, sc: 5), P: 4
