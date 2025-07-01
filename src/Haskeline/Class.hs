module Haskeline.Class
  ( -- * Entry-point
    runIO,

    -- * Class
    MonadHaskeline (..),
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Functor.Identity (Identity (Identity))
import GHC.Stack (HasCallStack)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

--------------------------------------------------------------------------------
--                                  Interface                                 --
--------------------------------------------------------------------------------

class (Monad m) => MonadHaskeline m where
  getInputLine :: (HasCallStack) => String -> m (Maybe String)

--------------------------------------------------------------------------------
--                                 Instances                                  --
--------------------------------------------------------------------------------

-- Not actually needed for this example.
instance (MonadIO m, MonadMask m) => MonadHaskeline (InputT m) where
  getInputLine = H.getInputLine

-- Needed ReaderT instance

-- This first instance is for common deriving i.e. our application type
-- is some transformer that wraps ReaderT.
instance {-# OVERLAPPABLE #-} (MonadHaskeline m) => MonadHaskeline (ReaderT e m) where
  getInputLine = lift . getInputLine

-- This second instance is what we use to actually provide the MonadHaskeline
-- instance. This utilizies the real Input logic and lifts it to ReaderT via
-- the new ReaderT API.
instance
  {-# OVERLAPS #-}
  (MonadIO m, MonadMask m) =>
  MonadHaskeline (ReaderT (InputTEnv m) m)
  where
  getInputLine = liftReaderT . H.getInputLine

-- Example mock instance

newtype MockIO a = MkMockIO a
  deriving stock (Functor)
  deriving (Applicative, Monad) via (Identity)

instance MonadHaskeline MockIO where
  getInputLine _ = pure $ Just "user input"

liftReaderT :: InputT m a -> ReaderT (InputTEnv m) m a
liftReaderT = HR.toReaderT

--------------------------------------------------------------------------------
--                                 Core logic                                 --
--------------------------------------------------------------------------------

runApp :: (MonadHaskeline m, MonadLogger m) => m ()
runApp = do
  name <-
    getInputLine "Enter your name: " >>= \case
      Nothing -> error "Needed a name!"
      Just n -> pure n
  printStr $ "Class: " ++ name

--------------------------------------------------------------------------------
--                              Application Type                              --
--------------------------------------------------------------------------------

newtype AppT e m a = MkAppT (ReaderT e m a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadIO,
      MonadLogger,
      MonadMask,
      MonadThrow
    )

deriving newtype instance (MonadHaskeline m) => MonadHaskeline (AppT e m)

runAppT :: AppT e m a -> e -> m a
runAppT (MkAppT m) e = runReaderT m e

--------------------------------------------------------------------------------
--                               Other classes                                --
--------------------------------------------------------------------------------

class (Monad m) => MonadLogger m where
  printStr :: String -> m ()

instance MonadLogger IO where
  printStr = putStrLn

instance (MonadLogger m) => MonadLogger (ReaderT e m) where
  printStr = lift . printStr

--------------------------------------------------------------------------------
--                                    Main                                    --
--------------------------------------------------------------------------------

-- Works! Because there is no InputT in the stack, it cannot interfere with
-- derived instances.
runIO :: IO ()
runIO = H.runInputT H.defaultSettings $ HR.fromReaderT $ runAppT runApp ()

-- Trying: AppT () (InputT IO) ()
-- No instance for ‘MonadLogger (InputT IO)’
-- runBad1 :: IO ()
-- runBad1 = H.runInputT H.defaultSettings (runAppT runApp ())

-- Trying: InputT (AppT () IO) ()
-- No instance for ‘MonadLogger (InputT (AppT () IO))’
-- runBad2 :: IO ()
-- runBad2 = runAppT (H.runInputT H.defaultSettings runApp) ()
