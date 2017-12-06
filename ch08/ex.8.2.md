# Excercise 8.2

`f` の長さが $m$ 、`r` の長さが $m+1$ のときに回転が始まり、 `f` の長さが $2m+1$ 、`r` の長さが $0$ になったとする。

次に回転が始まるのは `snoc` が $2m+2$ 回呼び出されたときなので、 つねに $1$ 回ずつ `exec` を呼び出せば、次の回転が始まるまでに回転操作は完了する。

また `f` が空になるのは $m$ 回の `tail` が呼ばれたときで、 `tail` によって `exec` と `invalidate` が $1$ 回ずつ呼ばれるので、回転が始まるときに $2$ 回 `exec` を呼び出せば `exec` と `invalidate` が合わせて $2m+2$ 回呼び出され、 `f` が空になるまでに回転操作が完了する。