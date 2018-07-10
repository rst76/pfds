# Exercise 10.5 (b)

The costs of `empty`, `isEmpty`, `head` are all obviously $O(1)$.
`snoc` and `tail` call `checkQ`, and `checkQ` calls `PrimQ.snoc` and `checkF`.
As forcing `$rev` in `checkF` takes $O(1)$ amortized time and `PrimQ.head` and `PrimQ.tail` take $O(1)$ worst-case time, `checkF` takes $O(1)$ amortized time in total.
`PrimQ.snoc` also costs $O(1)$, `checkQ` and therefore `snoc` and `tail` take $O(1)$ amortized time.
