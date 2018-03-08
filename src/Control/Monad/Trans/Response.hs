{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Trans.Response
  ( ResponseT(..)
  , runResponseT
  , runResponseT'
  , describeResponse
  , mapErrorResponse
  , throw
  , catch
  ) where

import           Alerta.Response

import           Control.Applicative       (Alternative (..), liftA2)
import           Control.Monad             (MonadPlus (..))
#if MIN_VERSION_base(4,9,0)
import           Control.Monad.Fail        (MonadFail)
import qualified Control.Monad.Fail        as MonadFail
#endif
import           Control.Monad.Fix         (MonadFix (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
#if MIN_VERSION_base(4,4,0)
import           Control.Monad.Zip         (MonadZip (mzipWith))
#endif
import           Data.Functor.Compose      (Compose (..), getCompose)
import           Data.Monoid               (mappend)
import           Data.Text                 (Text)

--------------------------------------------------------------------------------
-- Response monad transformer
--
-- Isomorphic to ExceptT Text
--------------------------------------------------------------------------------

newtype ResponseT m a = ResponseT { unResponseT :: m (Response a) }

runResponseT :: Functor m => ResponseT m a -> m (Either Text a)
runResponseT = runResponseT' Left Right

runResponseT' :: Functor m => (Text -> b) -> (a -> b) -> ResponseT m a -> m b
runResponseT' err ok = fmap (foldResponse err ok) . unResponseT

describeResponse :: Monad m => Text -> ResponseT m a -> ResponseT m a
describeResponse = mapErrorResponse . mappend

mapErrorResponse :: Functor m => (Text -> Text) -> ResponseT m a -> ResponseT m a
mapErrorResponse = onResponse . mapErrorText

onResponse :: Functor m => (Response a -> Response b) -> ResponseT m a -> ResponseT m b
onResponse f = ResponseT . fmap f . unResponseT

instance Functor m => Functor (ResponseT m) where
  fmap = onResponse . fmap
  {-# INLINE fmap #-}

instance Foldable m => Foldable (ResponseT m) where
  foldMap f (ResponseT m) = foldMap (foldResponse (const mempty) f) m
  {-# INLINE foldMap #-}

instance Traversable m => Traversable (ResponseT m) where
  traverse f (ResponseT m) = ResponseT <$>
    traverse (foldResponse (pure . Error) (fmap Ok . f)) m
  {-# INLINE traverse #-}

instance Applicative m => Applicative (ResponseT m) where
  pure = ResponseT . pure . Ok
  {-# INLINE pure #-}
  ResponseT f <*> ResponseT v = ResponseT $ getCompose $ Compose f <*> Compose v
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (ResponseT m) where
  empty = mzero
  {-# INLINE empty #-}
  (<|>) = mplus
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ResponseT m) where
  return = ResponseT . return . Ok
  {-# INLINE return #-}

  ResponseT m >>= k = ResponseT $
   m >>= foldResponse (return . Error) (unResponseT . k)

  fail = ResponseT . fail
  {-# INLINE (>>=) #-}

#if MIN_VERSION_base(4,9,0)
instance MonadFail m => MonadFail (ResponseT m) where
  fail = ResponseT . MonadFail.fail
  {-# INLINE fail #-}
#endif

instance Monad m => MonadPlus (ResponseT m) where
  mzero = ResponseT . return . Error $ ""
  {-# INLINE mzero #-}
  ResponseT ma `mplus` ResponseT mb = ResponseT $
    ma >>= foldResponse (const mb) (return . Ok)
  {-# INLINEABLE mplus #-}

instance MonadFix m => MonadFix (ResponseT m) where
  mfix f = ResponseT (mfix (unResponseT . f . foldResponse (const eek) id))
    where eek = error "mfix (ResponseT): inner computation returned Error"
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ResponseT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans ResponseT where
  lift = ResponseT . fmap Ok
  {-# INLINE lift #-}

#if MIN_VERSION_base(4,4,0)
instance MonadZip m => MonadZip (ResponseT m) where
  mzipWith f (ResponseT a) (ResponseT b) = ResponseT $ mzipWith (liftA2 f) a b
  {-# INLINE mzipWith #-}
#endif

throw :: Applicative m => Text -> ResponseT m a
throw = ResponseT . pure . Error

catch :: Monad m => ResponseT m a -> (Text -> ResponseT m a) -> ResponseT m a
ResponseT ma `catch` h = ResponseT $
  ma >>= foldResponse (unResponseT . h) (return . Ok)
