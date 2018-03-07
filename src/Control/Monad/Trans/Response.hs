{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Trans.Response where

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

newtype ResponseT m a = ResponseT { runResponse :: m (Response a) }

runResponse' :: Functor m => (Text -> b) -> (a -> b) -> ResponseT m a -> m b
runResponse' err ok = fmap (foldResponse err ok) . runResponse

describe :: Monad m => Text -> ResponseT m a -> ResponseT m a
describe t = mapErrorResponse (mappend t)

mapErrorResponse :: Functor m => (Text -> Text) -> ResponseT m a -> ResponseT m a
mapErrorResponse f = ResponseT . fmap (mapErrorText f) . runResponse

instance Functor m => Functor (ResponseT m) where
  fmap f = ResponseT . fmap (fmap f) . runResponse
  {-# INLINE fmap #-}

instance Foldable m => Foldable (ResponseT m) where
  foldMap f (ResponseT m) = foldMap (foldResponse (const mempty) f) m
  {-# INLINE foldMap #-}

instance Traversable m => Traversable (ResponseT m) where
  traverse f (ResponseT m) = ResponseT <$>
    traverse (foldResponse (pure . ErrorResponse) (fmap OkResponse . f)) m
  {-# INLINE traverse #-}

instance Applicative m => Applicative (ResponseT m) where
  pure = ResponseT . pure . OkResponse
  {-# INLINE pure #-}
  ResponseT f <*> ResponseT v = ResponseT $ getCompose $ Compose f <*> Compose v
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (ResponseT m) where
  empty = mzero
  {-# INLINE empty #-}
  (<|>) = mplus
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ResponseT m) where
  return = ResponseT . return . OkResponse
  {-# INLINE return #-}

  m >>= k = ResponseT $ do
    runResponse m >>=
      foldResponse (return . ErrorResponse) (runResponse . k)

  fail = ResponseT . fail
  {-# INLINE (>>=) #-}

#if MIN_VERSION_base(4,9,0)
instance MonadFail m => MonadFail (ResponseT m) where
  fail = ResponseT . MonadFail.fail
  {-# INLINE fail #-}
#endif

instance Monad m => MonadPlus (ResponseT m) where
  mzero = ResponseT . return . ErrorResponse $ ""
  {-# INLINE mzero #-}
  ResponseT ma `mplus` ResponseT mb = ResponseT $ do
    ma >>= foldResponse (const mb) (return . OkResponse)
  {-# INLINEABLE mplus #-}

instance MonadFix m => MonadFix (ResponseT m) where
  mfix f = ResponseT (mfix (runResponse . f . foldResponse (const eek) id))
    where eek = error "mfix (ResponseT): inner computation returned ErrorResponse"
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (ResponseT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans ResponseT where
  lift = ResponseT . fmap OkResponse
  {-# INLINE lift #-}

#if MIN_VERSION_base(4,4,0)
instance MonadZip m => MonadZip (ResponseT m) where
  mzipWith f (ResponseT a) (ResponseT b) = ResponseT $ mzipWith (liftA2 f) a b
  {-# INLINE mzipWith #-}
#endif

throw :: Applicative m => Text -> ResponseT m a
throw = ResponseT . pure . ErrorResponse

catch :: Monad m => ResponseT m a -> (Text -> ResponseT m a) -> ResponseT m a
ResponseT ma `catch` h = ResponseT $ do
  ma >>= foldResponse (runResponse . h) (return . OkResponse)
