# Exercise 5.9

Following operations' actual costs exceed the amortized bounds.

| Object (size $n$) | Operation ($d$ times parallel) | Amortized cost | Actual cost |
|--------|--------|--------|--------|
| binomial heap| insert | $O(d)$ | $O(d\log(n))$ |
| splay heap | insert | $O(d\log(n))$ | $O(dn)$ |
| pairing heap | deleteMin | $O(d\log(n))$ | $O(dn)$ |
