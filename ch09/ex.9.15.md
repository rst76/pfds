# Exercise 9.15

We show that for a skew binomial tree of size $|t|$ and rank $r$, $2^{r} \leq |t| \leq 2^{r+1}-1$.

When $r = 0$, $|t_{0}| = 1$ and it satisfies $2^{0} \leq 1 \leq 2^{0+1}-1$.

If $2^{k} \leq |t_{k}| \leq 2^{k+1}-1\ (r=k)$,

$\displaystyle \sum_{i=0}^{k}{2^{i}+1} \leq |t_{k+1}| \leq \sum_{i=0}^{k}(2^{i+1}-1)+1+(k+1)$

$\displaystyle 2^{k+1} \leq |t_{k+1}| \leq 2^{(k+1)+1}-1\ (r=k+1)$

So for any $r$, $2^{r} \leq |t| \leq 2^{r+1}-1$.