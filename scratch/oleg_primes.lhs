implify the previously posted genuine sieve algorithm and
generalize it to the finding of lucky numbers. 

We observe that we only need to store marks _signifying_ the integers,
but never the integers themselves. Thus we arrive at the algorithm
that is distinguished from all previously posted by:
  (i) doing no operations on composite numbers
  (ii) using neither multiplication nor division nor the
remainder operation
  (iii) using neither general addition nor the comparison.

The algorithm only relies on the successor, predecessor and zero
comparison. The predecessor can be easily eliminated. Thus the
algorithm can be used with Church and Peano numerals, or members of
Elliptic rings, where zero comparison and successor take constant
time but other arithmetic operations are more involved.

> -- repl_every_n n l replaces every (n+1)-th element in a list (_:l)
> -- with False
> repl_every_n :: Int -> [Bool] -> [Bool]
> repl_every_n n l = repl_every_n' n l
>  where repl_every_n' 0 (_:t) = False: repl_every_n n t
>        repl_every_n' i (h:t) = h:     repl_every_n' (pred i) t
>
> primes = 2:(loop 3 (repeat True))
>  where loop n (False:t) = loop (succ (succ n)) t
>        loop n (_:t)  = n:(loop (succ (succ n)) (repl_every_n (pred n) t))
>
> main = putStrLn $ "Last 10 primes less than 10000: " ++ 
>        show (take 10 $ reverse  $ takeWhile (< 10000) primes)

Last 10 primes less than 10000: 
 [9973,9967,9949,9941,9931,9929,9923,9907,9901,9887]

The algorithm easily generalizes to lucky numbers
  http://mathworld.wolfram.com/LuckyNumber.html
  http://www.research.att.com/~njas/sequences/A000959

