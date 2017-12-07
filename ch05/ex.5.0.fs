module BatchedQueue

type Queue<'a> = 'a list * 'a list

let empty<'a> : Queue<'a> = ([], [])

let check (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], r) -> (List.rev r, [])
    | _ -> q

let snoc ((f, r) : Queue<'a>, x : 'a) : Queue<'a> =
    check (f, x :: r)

let head (q : Queue<'a>) : 'a =
    match q with
    | ([], _) -> failwith "empty queue"
    | (x :: _, _) -> x

let tail (q : Queue<'a>) : Queue<'a> =
    match q with
    | ([], _) -> failwith "empty queue"
    | (_ :: f, r) -> check (f, r)
