# Meg's Challenge Problem

Given positive integers $n$ and $k$, create a vector $v$ with length $k$ by sampling the values $1:k$ with replacement. What is the probability that the sum of $v$ is equal to $n$?
 
Example: $n = 6, k = 3$

Answer: There are 7 vectors we can construct that sum to 6. They are $(1,2,3), (1,3,2), (2,1,3), (2,3,1), (3,1,2), (3,2,1), (2,2,2)$. 

There are 27 possible vectors that we can create, so the probability is 7/27.
 
**What is the probability that the sum of $v$ is equal to $n$, given $n = 5050$ and $k = 100$?**

## FFT solutions

Make use of ideas for [Robertson, Computation of Aggregate Loss Distributions](https://www.casact.org/sites/default/files/database/proceed_proceed92_92057.pdf)
