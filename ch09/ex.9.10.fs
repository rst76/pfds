module RandomAccessList

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Tree<'a> = Leaf of 'a | Node of Tree<'a> * Tree<'a>
type Digit<'a> = One of Tree<'a> | Two of Tree<'a> * Tree<'a> | Three of Tree<'a> * Tree<'a> * Tree<'a>
type Schedule<'a> = Stream<Digit<'a>> list
type RList<'a> = Stream<Digit<'a>> * Schedule<'a>

let empty<'a> : RList<'a> = (lazy Nil, [])

let rec insTree (t : Tree<'a>, Lazy s : Stream<Digit<'a>>) : Stream<Digit<'a>> =
    match s with
    | Nil -> lazy (Cons (One t, lazy Nil))
    | Cons (Zero, ds) -> lazy (Cons (One t, ds))
    | Cons (One t', ts') ->
        lazy (Cons (Zero, insTree (link (t, t'), ts')))

let exec (s : Schedule<'a>) : Schedule<'a> =
    match s with
    | [] -> []
    | Lazy (Cons (One t, _)) :: sched -> sched
    | Lazy (Cons (Zero, job)) :: sched -> job :: sched

let insert (x : 'a, (ds, sched) : Heap<'a>) : Heap<'a> =
    let ds' = insTree (Node (x, []), ds)
    (ds', exec (exec (ds' :: sched)))

let rec mrg (dss : Stream<Digit<'a>> * Stream<Digit<'a>>) : Stream<Digit<'a>> =
    match dss with
    | (ds1, Lazy Nil) -> ds1
    | (Lazy Nil, ds2) -> ds2
    | (Lazy (Cons (Zero, ds1)), Lazy (Cons (d, ds2))) -> lazy (Cons (d, mrg (ds1, ds2)))
    | (Lazy (Cons (d, ds1)), Lazy (Cons (Zero, ds2))) -> lazy (Cons (d, mrg (ds1, ds2)))
    | (Lazy (Cons (One t1, ds1)), Lazy (Cons (One t2, ds2))) ->
        lazy (Cons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))

let rec normalize (ds : Stream<Digit<'a>>) : Stream<Digit<'a>> =
    match ds with
    | Lazy Nil -> ds
    | Lazy (Cons (_, ds')) -> normalize ds' |> ignore; ds

let merge (tss : Stream<Digit<'a>> * Stream<Digit<'a>>) : Heap<'a> =
    (normalize (mrg tss), [])

let rec unconsTree (Lazy ts : Stream<Digit<'a>>) : Tree<'a> * Stream<Digit<'a>> =
    match ts with
    | Nil -> failwith "empty heap"
    | Cons (One t, Lazy Nil) -> t
    | Cons (Two (t, t1), ts) -> (t, lazy (Cons (One t1, ts)))
    | Cons (Three (t, t1, t2), ts) -> (t, lazy (Cons (Two (t1, t2), ts)))
    | Cons (One t, ts) ->
        let (Node (t1, t2), ts') = unconsTree ts
        (t, lazy (Cons (Two (t1, t2), ts')))

let head ((ts, s) : RList<'a>) : 'a =
    let (Leaf x, _) = unconsTree ts in x

let tail ((ts, s) : RList<'a>) : RList<'a> =
    let (_, ts') = unconsTree ts in ts'
