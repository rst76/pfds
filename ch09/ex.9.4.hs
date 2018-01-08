import Prelude hiding ((+))

data Digit = One | Two deriving Show
type Nat = [Digit]

inc :: Nat -> Nat
inc [] = [One]
inc (One : ds) = Two : ds
inc (Two : ds) = One : inc ds

dec :: Nat -> Nat
dec [] = error "decrimenting zero"
dec [One] = []
dec (One : ds) = Two : dec ds
dec (Two : ds) = One : ds

(+) :: Nat -> Nat -> Nat
ds + [] = ds
[] + ds = ds
(One : ds1) + (One : ds2) = Two : (ds1 + ds2)
(d : ds1) + (Two : ds2) = d : inc (ds1 + ds2)
(Two : ds1) + (d : ds2) = d : inc (ds1 + ds2)
