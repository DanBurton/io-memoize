-- | Memoize IO actions,
-- performing them at most once,
-- but recalling their result for subsequent invocations.
-- This library provides three sequencing strategies:
-- lazy ('ioMemo'), eager ('ioMemo''), and concurrent ('ioMemoPar').
-- 
-- The lazy and eager approaches give stronger sequencing guarantees.
-- 
-- The following property holds: @join . ioMemo === id@.
-- The same is true for @ioMemo'@ and @ioMemoPar@.
-- 
-- Also, for the three memoizers in this library,
-- the memory allocated for the result will not
-- be available for garbage collection until the corresponding
-- memoized action is also available for garbage collection,
-- unless your compiler performs deep magicks.
module System.IO.Memoize (
    ioMemo
  , ioMemo'
  , ioMemoPar
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Data.IORef

-- | Memoize an IO action.
-- The action will be performed
-- the first time that it its value is demanded;
-- all subsequent invocations
-- will simply recall the value acquired
-- from the first call.
-- If the value is never demanded,
-- then the action will never be performed.
-- 
-- This is basically a safe version of
-- 'System.IO.Unsafe.unsafeInterleaveIO'.
-- This function is also thread-safe:
-- it is guaranteed that the action passed in
-- will be performed exactly 0 or 1 times
-- by this code. Exceptions will be propagated
-- to the caller.
-- 
-- Example usage:
-- 
-- >>> getLine' <- ioMemo getLine
-- 
-- >>> replicateM 3 getLine'
-- Hello
-- ["Hello", "Hello", "Hello"]
ioMemo :: IO a -> IO (IO a)
ioMemo action = do
  memo <- newIOMemoizer
  return (memo action)

-- | Memoize an IO action.
-- The action will be performed immediately;
-- all subsequent invocations
-- will recall the value acquired.
ioMemo' :: IO a -> IO (IO a)
ioMemo' action = do
  v <- action
  return (return v)

-- | Memoize an IO action.
-- The action will be performed immediately in a new thread.
-- Attempts to access the result
-- will block until the action is finished.
ioMemoPar :: IO a -> IO (IO a)
ioMemoPar action = do
  thread <- async action
  return (wait thread)



newIOMemoizer :: IO (IO a -> IO a)
newIOMemoizer = do
  b <- newMVar True
  r <- newIORef undefined
  return (ioMemoizer b r)

ioMemoizer :: MVar Bool -> IORef a -> IO a -> IO a
ioMemoizer b r action = do
  modifyMVar_ b $ \isEmpty ->
    if isEmpty
      then do v <- action
              writeIORef r v
              return False
      else return False
  readIORef r
