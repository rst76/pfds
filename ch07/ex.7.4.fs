module ScheduledBinomialHeap

type StreamCell<'a> = Nil | Cons of 'a * Stream<'a>
and Stream<'a> = Lazy<StreamCell<'a>>
type Tree<'a> = Node of 'a * List<Tree<'a>>
type Digit<'a> = Zero | One of Tree<'a>
type Schedule<'a> = List<Stream<Digit<'a>>>
type Heap<'a> = Stream<Digit<'a>> * Schedule<'a>

let empty<'a> = (lazy Nil, [])

let isEmpty (Lazy s, _) =
  match s with
    | Nil -> true
    | _ -> false

let link (Node (x1, c1) as t1, (Node (x2, c2) as t2)) =
  if x1 <= x2 then Node (x1, t2 :: c1) else Node (x2, t1 :: c2)

let rec insTree (t, Lazy s) =
  match s with
    | Nil -> lazy (Cons (One t, lazy Nil))
    | Cons (Zero, ds) -> lazy (Cons (One t, ds))
    | Cons (One t', ts') ->
      lazy (Cons (Zero, insTree (link (t, t'), ts')))

let exec s =
  match s with
    | [] -> []
    | Lazy (Cons (One t, _)) :: sched -> sched
    | Lazy (Cons (Zero, job)) :: sched -> job :: sched

let insert (x, (ds, sched)) =
  let ds' = insTree (Node (x, []), ds)
  (ds', exec (exec (ds' :: sched)))

let rec mrg dss =
  match dss with
    | (ds1, Lazy Nil) -> ds1
    | (Lazy Nil, ds2) -> ds2
    | (Lazy (Cons (Zero, ds1)), Lazy (Cons (d, ds2))) -> lazy (Cons (d, mrg (ds1, ds2)))
    | (Lazy (Cons (d, ds1)), Lazy (Cons (Zero, ds2))) -> lazy (Cons (d, mrg (ds1, ds2)))
    | (Lazy (Cons (One t1, ds1)), Lazy (Cons (One t2, ds2))) ->
      lazy (Cons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))

let rec normalize ds =
  match ds with
    | Lazy Nil -> ds
    | Lazy (Cons (_, ds')) -> normalize ds' |> ignore; ds

let merge (Lazy ts1, Lazy ts2) = (normalize (mrg (ts1, ts2)), [])

let rec removeMinTree (Lazy s) =
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

let findMin (ds, _) = let (Node (x, _), _) = removeMinTree ds in x

let rec mrgWithList s =
  match s with
    | ([], ds) -> ds
    | (t :: ts, Lazy Nil) -> lazy (Cons (One t, mrgWithList (ts, lazy Nil)))
    | (t :: ts, Lazy (Cons (Zero, ds))) -> lazy (Cons (One t, mrgWithList (ts, ds)))
    | (t1 :: ts, Lazy (Cons (One t2, ds))) ->
      lazy (Cons (Zero, insTree (link (t1, t2), mrgWithList (ts, ds))))

let deleteMin (ds, _) =
  let (Node (_, c), ds') = removeMinTree ds
  let ds'' = mrgWithList (List.rev c, ds')
  (normalize ds'', [])
