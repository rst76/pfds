Every `cons` and `snoc` without balancing (reverse) takes 1 step and increase or decrease $\Phi$ by 1.
And `tail` and `init` without balancing also takes 1 step and increase or decrease $\Phi$ by 1.

`cons` and `snoc` with balancing is called only at the first time. it takes 2 steps and increases $\Phi$ by 1. The amortized cost $a_i =2+1=3$. (In my implementation, it is called at the second time, takes 3 steps and decreses $\Phi$ by 1, but anyway the number of operations is constant and the conclusion is same.)

Consider `tail` with balancing, let the size of rear list as $m$. It takes $m$ steps to reconstruct the two lists and decreases $\Phi_{i-1}=m-1$ to $\Phi_i=0$, so the amortized cost $a_i=m+1-(m-1)=2$.
It is also true for `init` with balancing, so each operation takes $O(1)$ amortized time.
