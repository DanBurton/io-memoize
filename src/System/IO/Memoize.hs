-- | Memoize IO actions, performing them at most once,
-- but recalling their result for subsequent invocations.
-- This library provides two sequencing strategies:
-- lazy ('once'), and concurrent ('eagerlyOnce').
-- 
-- The following property holds: @join . once === id@.
-- The same is true for @eagerlyOnce@.
-- 
-- The memory allocated for a memoized result will obviously not
-- be available for garbage collection until the corresponding
-- memoized action is also available for garbage collection.
module System.IO.Memoize (
    once
  , eagerlyOnce
  , withEagerlyOnce
  , ioMemo
  , ioMemo'
  , ioMemoPar
  ) where

import Control.Concurrent.Async (async, withAsync, wait)
import Control.Concurrent.Cache (newCache, fetch)

-- | Memoize an IO action. The action will be performed
-- the first time that it its value is demanded;
-- all subsequent invocations will simply recall the value acquired
-- from the first call. If the value is never demanded,
-- then the action will never be performed.
-- 
-- This is basically a safe version of 'System.IO.Unsafe.unsafeInterleaveIO'.
-- Exceptions will be propagated to the caller, and the action will be retried
-- at each invocation, only until it has successfully completed once.
-- 
-- Example usage:
-- 
-- >>> getLine' <- once getLine
-- 
-- >>> replicateM 3 getLine'
-- Hello
-- ["Hello", "Hello", "Hello"]
once :: IO a -> IO (IO a)
once action = do
  cache <- newCache
  return (fetch cache action)

-- | Memoize an IO action.
-- The action will be started immediately in a new thread.
-- Attempts to access the result will block until the action is finished.
-- If the action produces an error, then attempts to access its value
-- will re-raise the same error each time.
eagerlyOnce :: IO a -> IO (IO a)
eagerlyOnce action = do
  thread <- async action
  return (wait thread)

-- | Memoize an 'IO' action. @withEagerlyOnce act cont@ will immediately start
-- a new thread to perform @act@. It will pass @cont@ an 'IO' action it can use
-- to retrieve the result of @act@. Once @cont@ has completed, @act@ will be
-- cancelled (using 'Control.Concurrent.Async.uninterruptibleCancel').  This
-- prevents a thread from continuing to run indefinitely when its result is no
-- longer needed.  The action passed to @cont@ should never be used after
-- @cont@ has completed.  Like 'eagerlyOnce', if @act@ fails, then the same
-- failure will be repeated on each attempted retrieval.
withEagerlyOnce :: IO a -> (IO a -> IO b) -> IO b
withEagerlyOnce action cont = withAsync action $ \thr ->
  cont (wait thr)

{-# DEPRECATED ioMemo' "Please just call the action directly." #-}
-- | Memoize an IO action.
-- The action will be performed immediately;
-- all subsequent invocations
-- will recall the value acquired.
ioMemo' :: IO a -> IO (IO a)
ioMemo' action = do
  v <- action
  return (return v)

{-# DEPRECATED ioMemo "Please use 'once'." #-}
ioMemo :: IO a -> IO (IO a)
ioMemo = once

{-# DEPRECATED ioMemoPar "Please use 'eagerlyOnce'." #-}
ioMemoPar :: IO a -> IO (IO a)
ioMemoPar = eagerlyOnce
