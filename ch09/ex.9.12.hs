data Digit = Zero | Ones Int | Two | Threes Int | Four deriving Show
type Nat = [Digit]

ones :: Int -> Nat -> Nat
ones 0 ds = ds
ones i (Ones j : ds) = Ones (i + j) : ds
ones i ds = Ones i : ds

threes :: Int -> Nat -> Nat
threes 0 ds = ds
threes i (Threes j : ds) = Threes (i + j) : ds
threes i ds = Threes i : ds

fixup :: Nat -> Nat
fixup (Threes i : ds) = Threes i : fixup ds
fixup (Four : ds) = Two : simpleInc ds
fixup ds = ds

simpleInc :: Nat -> Nat
simpleInc [] = [Ones 1]
simpleInc (Zero : ds) = ones 1 ds
simpleInc (Ones i : ds) = Two : ones (i - 1) ds
simpleInc (Two : ds) = threes 1 ds
simpleInc (Threes i : ds) = Four : threes (i - 1) ds

inc :: Nat -> Nat
inc = fixup . simpleInc

fixdown :: Nat -> Nat
fixdown (Ones i : ds) = Ones i : fixdown ds
fixdown (Zero : ds) = Two : simpleDec ds
fixdown ds = ds

simpleDec :: Nat -> Nat
simpleDec [] = error "decrimenting zero"
simpleDec [Ones 1] = []
simpleDec (Ones i : ds) = Zero : ones (i - 1) ds
simpleDec (Two : ds) = ones 1 ds
simpleDec (Threes i : ds) = Two : threes (i - 1) ds
simpleDec (Four : ds) = threes 1 ds

dec :: Nat -> Nat
dec = fixdown . simpleDec
