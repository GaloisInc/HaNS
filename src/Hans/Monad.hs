module Hans.Monad where


newtype Hans a = Hans { unHans :: IO () -> (a -> IO ()) -> IO () }

instance Functor Hans where
  fmap f m = Hans (\ e k -> unHans m e (k . f))
  {-# INLINE fmap #-}

instance Applicative Hans where
  pure x  = Hans (\ _ k -> k x)

  f <*> x = Hans $ \ e k -> unHans f e
                 $ \ g   -> unHans x e
                 $ \ y   -> k (g y)

  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Monad Hans where
  return  = pure

  m >>= f = Hans $ \ e k -> unHans m e
                 $ \ a   -> unHans (f a) e k

  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}


-- | Loop forever, running the underlying operation.
runHans :: Hans () -> IO ()
runHans (Hans m) = loop ()
  where
  loop () = m (loop ()) loop

{-# INLINE runHans #-}

