# Exercise 5.3

## merge

Consider merge for two heaps each having $a$, $b$ trees.

If there occurs no link, it takes $a + b$ steps and $\Phi(d_{in1}) = a, \Phi(d_{in2}) = b, \Phi(d_{out}) = a+b$. So amortized cost $a_i = (a+b)+(a+b)-(a+b) = a+b$. Let the size of merged heap as $n$, as $a < \lfloor\log(n+1)\rfloor, b < \lfloor\log(n+1)\rfloor$, it consists that $a+b < 2\lfloor\log(n+1)\rfloor$, and amortized cost is $O(\log n)$.

If there ocurs $k$ times link, merge takes $a+b+k$ steps and $\Phi(d_{in1}) = a, \Phi(d_{in2}) = b, \Phi(d_{out}) = a+b-k$. So amortized cost $a_i = (a+b+k)+(a+b-k)-(a+b) = a+b$ is also $O(\log n)$.

## deleteMin

Consider deleteMin for the heap that contains $a$ trees. Calls to removeMinTree and a call to rev of ts1 takes at most $a$ steps.
$a < \lfloor\log(n+1)\rfloor$, so removeMin and rev both takes $O(\log n)$ times.
As mentioned merge also takes $O(\log n)$, deleteMin takes $O(\log n)$ times.
