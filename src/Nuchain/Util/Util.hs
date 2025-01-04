{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nuchain.Util.Util
  ( DiagnosticException(..)
  , NotReadyException(..)
  , TrackedError(..)
  , awsDashVar
  , catchAndRethrow
  , fromMaybeM
  , foreverRetry
  , linkAsyncTrack
  , linkAsyncBoundTrack
  , seqIndex
  , throwDiagnostics
  ) where

import Control.Concurrent (forkIO, forkIOWithUnmask)
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (SomeAsyncException)
import Control.Monad
import Control.Monad.Catch
import Data.List
import Data.Maybe
import Data.String (IsString)
import Data.Typeable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Process (system)

foreverRetry :: (String -> IO ()) -> String -> IO () -> IO ()
foreverRetry debug threadName actions = threadName `deepseq` go
 where
  go = void $ uninterruptibleMask_ $ forkIOWithUnmask $ \unmask ->
    forever $
      unmask (forever actions) `catch` \(_ :: SomeException) ->
        debug $ threadName ++ "is being restarted"


awsDashVar :: Bool -> String -> String -> IO ()
awsDashVar False _ _ = return ()
awsDashVar True  k v = void $! forkIO $! void $! system $
  "aws ec2 create-tags --resources `ec2-metadata --instance-id | sed 's/^.*: //g'` --tags Key="
  ++ k
  ++ ",Value="
  ++ v
  ++ " >/dev/null"

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

fromMaybeM :: Monad m => m b -> Maybe b -> m b
fromMaybeM errM = maybe errM (return $!)

data TrackedError e = TrackedError
  { teTrace :: [String]
  , teError :: e
  } deriving (Eq, Ord, Typeable)

instance Show e => Show (TrackedError e) where
  show TrackedError{..} = "[" ++ intercalate "." teTrace ++ "] Uncaught Exception: " ++ show teError

instance (Exception e) => Exception (TrackedError e)

catchAndRethrow :: MonadCatch m => String -> m a -> m a
catchAndRethrow loc fn
  = fn `catches` [ Handler (\(e@TrackedError{..} :: TrackedError SomeException)
                   -> throwM $ e {teTrace = loc : teTrace})
                 , Handler (\(e :: SomeAsyncException)  -> throwM e)
                 , Handler (\(e :: SomeException)  -> throwM $ TrackedError [loc] e)]

-- | Run an action asynchronously on a new thread. If an uncaught exception is encountered in the
--   thread, capture it, track its location, and re-throw it to the parent thread.
--   This is useful for when you're not expecting an exception in a child thread and want to know
--   where to look after it's thrown.
linkAsyncTrack :: String -> IO a -> IO ()
linkAsyncTrack loc fn = link =<< (async $ catchAndRethrow loc fn)

linkAsyncBoundTrack :: String -> IO a -> IO ()
linkAsyncBoundTrack loc fn = link =<< (asyncBound $ catchAndRethrow loc fn)

throwDiagnostics :: MonadThrow m => Maybe Bool -> String -> m ()
throwDiagnostics mDiag str = do
  when (fromMaybe False mDiag) $
    throwM $ DiagnosticException str

newtype DiagnosticException = DiagnosticException String
  deriving (Eq,Show,Ord,IsString)
instance Exception DiagnosticException

newtype NotReadyException = NotReadyException String
  deriving (Eq,Show,Ord,IsString)
instance Exception NotReadyException
