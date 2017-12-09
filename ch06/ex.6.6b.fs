module BadPhysicistsQueue

type Queue<'a> = 'a list * int * int * Lazy<'a list> * int * 'a list

let empty<'a> : Queue<'a> = ([], 0, 0, lazy [], 0, [])

let isEmpty ((_, lenf, _, _, _, _) : Queue<'a>) : bool = lenf = 0

let checkw (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], lenf, skipf, f, lenr, r) ->
        let f' = List.skip skipf (f.Force())
        (f', lenf, 0, lazy f', lenr, r)
    | _ -> q

let check ((_, lenf, skipf, f, lenr, r) as q : Queue<'a>) : Queue<'a> =
    if lenr <= lenf then checkw q
    else let f' = List.skip skipf (f.Force())
         checkw (f', lenf + lenr, 0, lazy (f' @ List.rev r), 0, [])

let snoc ((w, lenf, skipf, f, lenr, r) : Queue<'a>, x : 'a) =
    check (w, lenf, skipf, f, lenr + 1, x :: r)

let head (q : Queue<'a>) : 'a =
    match q with
    | ([], _, _, _, _, _) -> failwith "empty queue"
    | (x :: _, _, _, _, _, _) -> x

let tail (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], _, _, _, _, _) -> failwith "empty queue"
    | (_ :: w, lenf, skipf, f, lenr, r) -> check (w, lenf - 1, skipf + 1, f, lenr, r)

let q0 = Seq.foldBack (fun _ q -> tail q) {1 .. 30}
            (Seq.fold (fun q x -> snoc (q, x)) empty {1 .. 31})
// => ([31], 1, 15, 値は作成されていません。, 0, [])
let q1 = snoc (q0, 1)
// => ([31], 1, 15, 値は作成されていません。, 1, [1])
let q2 = snoc (q0, 2)
// => ([31], 1, 15, 値は作成されていません。, 1, [2])
let q3 = snoc (q0, 3)
// => ([31], 1, 15, 値は作成されていません。, 1, [3])
tail q1
// => ([1], 1, 0, 値は作成されていません。, 0, [])
tail q2
// => ([2], 1, 0, 値は作成されていません。, 0, [])
tail q3
// => ([3], 1, 0, 値は作成されていません。, 0, [])
