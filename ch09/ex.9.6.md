# Exercise 9.6

lookup と update は対象の木を探すのに最大で $\lfloor \log (i + 1) + 1 \rfloor$ ステップ、その木から対象の要素を探すのに最大で $\lfloor \log (i + 1) + 2 \rfloor$ ステップかかる。

よってどちらも $O(\log i)$ 時間で動く。
