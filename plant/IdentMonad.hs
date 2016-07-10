
module IdentMonad where

import MonadLib


import IOExts


global_id :: IORef Int
global_id = unsafePerformIO (newIORef 0)

next_id = do
    n <- readIORef global_id
    writeIORef global_id (succ n)
    return n

data Ref a = Ref (Int,a)
 deriving Eq


mk_ref a = unsafePerformIO (do i <- next_id
                               return (i,a))
get_ref (Ref (_,r)) = r
get_id (Ref (i,_)) = i

lft m = IM (Ref (mk_ref m))

tell i m = IM (Ref (i,m))

eq_im (IM m) (IM m') = get_id m == get_id m'
cmp_im (IM m) (IM m') = get_id m `compare` get_id m'

im_id (IM m) = get_id m

data IdentMonad m a = IM (Ref (m a))

instance Eq (IdentMonad m a) where
    (==) = eq_im

instance Ord (IdentMonad m a) where
    compare = cmp_im

instance Monad m => Monad (IdentMonad m) where
    return v = IM (Ref (mk_ref (return v)))
    IM m >>= f = IM (Ref (mk_ref (get_ref m >>= \v -> let IM m' = f v in get_ref m' )))

run_im (IM m) = get_ref m





