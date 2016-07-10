
module MonadLib where

import Monad

class Monad m => MonadRec m where
    mfix :: (a -> m a) -> m a


data ContMonad p a = ContMonad {unCM :: (a -> p) -> p }

instance Monad (ContMonad p) where
	return v = ContMonad (\k -> k v)
	m >>= f  = ContMonad (\k -> unCM m (\v -> unCM (f v) k))



data StateMonadP s m a = ST { unST :: (s -> m (a,s)) }

instance Monad m => Monad (StateMonadP s m) where
    return v	= ST (\s -> return (v,s))
    ST m >>= f	= ST (\s -> do	(v,s') <- m s
				unST (f v) s')

class Monad m => UnMonad m where
    unM :: m a -> a

instance UnMonad IdMonad where
    unM = unIdM

instance UnMonad m => MonadRec (StateMonadP s m) where
    mfix f = ST (\s -> let (a,s') = unM (unST (f a) s)
                       in return (a,s'))
                          

type StateMonad s a = StateMonadP s IdMonad a
type IOState s a = StateMonadP s IO a


io :: IO a -> StateMonadP s IO a
io = liftm

liftm f = ST (\s -> do v <- f; return (v,s))


liftSt2 :: (a -> (b,c) -> m (e,c)) -> a -> b -> StateMonadP c m e
liftSt2 f a1 a2 = ST (\s -> f a1 (a2,s))

getst :: Monad m => StateMonadP s m s
getst		= ST (\s -> return (s,s))

getstp :: Monad m => (s -> b) -> StateMonadP s m b
getstp f	= liftM f getst

setst :: Monad m => a -> StateMonadP a m ()
setst s		= modst (\_ -> s)

modst :: Monad m => (s -> s) -> StateMonadP s m ()
modst f         = updst ((,) () . f)

updst :: Monad m => (s -> (a,s)) -> StateMonadP s m a
updst f         = ST (\s -> return (f s))

runst :: StateMonad s a -> s -> (a,s)
runst s i = unIdM ((unST s) i)

runstp :: StateMonadP s m a -> s -> m (a,s)
runstp s i = (unST s) i


data ErrorMonad a = Ok a | Fail String

instance Monad ErrorMonad where
	return v	= Ok v
	Ok   a >>= f	= f a
	Fail a >>= _	= Fail a

data IdMonad a = IdMonad { unIdM :: a }

instance Monad IdMonad where
	return a = IdMonad a
	IdMonad m >>= f = f m

class Monad m => RndGen m where
    rnd :: m Float
    rnds :: m [Float]


{-
class (Monad m, Monad (t m)) => MonadT t m where
    lift :: m a -> t m a

instance Monad m => MonadT (StateMonadP a) m where
    lift m = ST (\s -> m >>= \v -> return (v,s))
-}
