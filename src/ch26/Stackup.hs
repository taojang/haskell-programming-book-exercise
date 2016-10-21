module Stackup where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT fab) <*> (EitherT mea) = EitherT $ (<*>) <$> fab <*> mea


instance Monad m => Monad (EitherT e m) where
  return =  pure

  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        (Left e)  -> pure (Left e)
        (Right a) -> runEitherT (f a)


swapEitherT :: (Functor m)  => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swap . runEitherT
  where swap (Left x)  = Right x
        swap (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT mab) =
  mab >>= (\x ->
             case x of
               (Left a)  -> fa a
               (Right b) -> fb b)


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    (\(a, s') -> (f a, s')) <$> sma s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, s')  <- smf s
    (a, s'') <- sma s'
    pure (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
