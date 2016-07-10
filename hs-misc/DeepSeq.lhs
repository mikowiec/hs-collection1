Enforcing Strict Evaluation 

Dean Herington heringto@cs.unc.edu 
Fri, 17 Aug 2001 16:41:24 -0400 (EDT) 


The prelude support for strict evaluation, `seq` and ($!), evaluate
only enough to ensure that the value being "forced" is not bottom.  In
your case you need a "deeper" evaluation to be forced.

A clean (though somewhat tedious) way to achieve what you need is with
the `deepSeq` function from the following module.

Dean Herington



The `DeepSeq` class provides a method `deepSeq` that is similar to
`seq` except that it forces deep evaluation of its first argument
before returning its second argument.

Instances of `DeepSeq` are provided for Prelude types.  Other
instances must be supplied by users of this module.

> module DeepSeq where

> class  DeepSeq a  where
>   deepSeq :: a -> b -> b
>   deepSeq = seq                       -- default, for simple cases

> infixr 0 `deepSeq`, $!!

> ($!!) :: (DeepSeq a) => (a -> b) -> a -> b
> f $!! x = x `deepSeq` f x


> instance  DeepSeq ()  where

> instance  (DeepSeq a) => DeepSeq [a]  where
>   deepSeq [] y = y
>   deepSeq (x:xs) y = deepSeq x $ deepSeq xs y

> instance  (DeepSeq a,DeepSeq b) => DeepSeq (a,b)  where
>   deepSeq (a,b) y = deepSeq a $ deepSeq b y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c) => DeepSeq (a,b,c)  where
>   deepSeq (a,b,c) y = deepSeq a $ deepSeq b $ deepSeq c y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) => 
>           DeepSeq (a,b,c,d)  where
>   deepSeq (a,b,c,d) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e) => 
>           DeepSeq (a,b,c,d,e)  where
>   deepSeq (a,b,c,d,e) y =   deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d 
>                           $ deepSeq e y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f) => 
>           DeepSeq (a,b,c,d,e,f)  where
>   deepSeq (a,b,c,d,e,f) y =   deepSeq a $ deepSeq b $ deepSeq c 
>                             $ deepSeq d $ deepSeq e $ deepSeq f y
> instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f,
>            DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)  where
>   deepSeq (a,b,c,d,e,f,g) y =   deepSeq a $ deepSeq b $ deepSeq c 
>                               $ deepSeq d $ deepSeq e $ deepSeq f 
>                               $ deepSeq g y

> instance  DeepSeq Bool  where
> instance  DeepSeq Char  where

> instance  (DeepSeq a) => DeepSeq (Maybe a)  where
>   deepSeq Nothing y = y
>   deepSeq (Just x) y = deepSeq x y

> instance  (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)  where
>   deepSeq (Left a) y = deepSeq a y
>   deepSeq (Right b) y = deepSeq b y

> instance  DeepSeq Ordering  where

> instance  DeepSeq Integer  where
> instance  DeepSeq Int  where
> instance  DeepSeq Float  where
> instance  DeepSeq Double  where
