# Exercise 3.1

Let the size of the leftist heap of rank $d$ as $n$.

If $n < 2^d - 1$, it doesn't satisfy the requirement of leftist heap. So,

$2^d - 1 <= n$

$2^d <= n + 1$

$d <= \log(n + 1)$

For $d$ is an integer,

$d <= \lfloor\log(n + 1)\rfloor$
