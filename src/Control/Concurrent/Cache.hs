module Control.Concurrent.Cache (Cache, newCache, fetch) where

import Control.Concurrent.MVar
import Data.IORef

-- | A thread-safe write-once cache.
newtype Cache a = Cache {
  -- | Fetch the value stored in the cache,
  -- or call the supplied fallback and store the result,
  -- if the cache is empty.
  fetch :: IO a -> IO a
}

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = do
  b <- newMVar True
  r <- newIORef undefined
  return (cache b r)

cache :: MVar Bool -> IORef a -> Cache a
cache b r = Cache $ \action -> do
  modifyMVar_ b $ \isEmpty ->
    if isEmpty
      then do v <- action
              writeIORef r v
              return False
      else return False
  readIORef r
