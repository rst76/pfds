module ScheduledBinomialHeap

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Tree<'a> = Node of 'a * Tree<'a> list
type Digit<'a> = Zero | One of Tree<'a>
type Schedule<'a> = Stream<Digit<'a>> list
type Heap<'a> = Stream<Digit<'a>> * Schedule<'a>

let empty<'a> : Heap<'a> = (lazy Nil, [])

let isEmpty ((Lazy s, _) : Heap<'a>) : bool =
  match s with
    | Nil -> true
    | _ -> false

let link (Node (x1, c1) as t1 : Tree<'a>, (Node (x2, c2) as t2 : Tree<'a>)) : Tree<'a> =
  if x1 <= x2 then Node (x1, t2 :: c1) else Node (x2, t1 :: c2)

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

let rec removeMinTree (Lazy s : Stream<Digit<'a>>) =
  match s with
    | Nil -> failwith "empty heap"
    | Cons (One t, Lazy Nil) -> (t, lazy Nil)
    | Cons (Zero, ds) ->
      let (t', ds') = removeMinTree ds
      (t', lazy (Cons (Zero, ds')))
    | Cons (One (Node (x, _) as t), ds) ->
      let (Node (x', _) as t', ds') = removeMinTree ds
      if x <= x' then (t, lazy (Cons (Zero, ds)))
      else (t', lazy (Cons (One t, ds')))

let findMin ((ds, _) : Heap<'a>) : 'a =
  let (Node (x, _), _) = removeMinTree ds in x

let rec mrgWithList (s : Tree<'a> list * Stream<Digit<'a>>) : Stream<Digit<'a>> =
  match s with
    | ([], ds) -> ds
    | (t :: ts, Lazy Nil) -> lazy (Cons (One t, mrgWithList (ts, lazy Nil)))
    | (t :: ts, Lazy (Cons (Zero, ds))) -> lazy (Cons (One t, mrgWithList (ts, ds)))
    | (t1 :: ts, Lazy (Cons (One t2, ds))) ->
      lazy (Cons (Zero, insTree (link (t1, t2), mrgWithList (ts, ds))))

let deleteMin ((ds, _) : Heap<'a>) : Heap<'a> =
  let (Node (_, c), ds') = removeMinTree ds
  let ds'' = mrgWithList (List.rev c, ds')
  (normalize ds'', [])
