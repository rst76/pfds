# Exercise 10.6

From thorem 10.1, `(++)` runs in $O(1)$.
For the list of length $n$, besides the amortized cost $Cn$,
it costs $e$ in appending the empty lists.
So `flatten` runs in $O(1+e/n) \leq O(1+e)$ amortized time.
