{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Haskeline.Effectful.Dynamic
  ( -- * Entry-point
    runIO,

    -- * Effect
    Haskeline (..),
    getInputLine,

    -- ** Handlers
    runHaskeline,
    runHaskelineMock,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (ReaderT))
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.Reader.Static (Reader, runReader)
import Haskeline.Effectful.Static qualified as Static
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

--------------------------------------------------------------------------------
--                              Dynamic Effect                                --
--------------------------------------------------------------------------------

type instance DispatchOf Haskeline = Dynamic

data Haskeline :: Effect where
  GetInputLine :: String -> Haskeline m (Maybe String)

getInputLine :: (Haskeline :> es) => String -> Eff es (Maybe String)
getInputLine = send . GetInputLine

--------------------------------------------------------------------------------
--                                 Handlers                                   --
--------------------------------------------------------------------------------

-- real handler
runHaskeline ::
  ( IOE :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  Eff (Haskeline : es) a ->
  Eff es a
runHaskeline = reinterpret_ Static.runHaskeline $ \case
  GetInputLine s -> Static.getInputLine s

-- Example mock handler
runHaskelineMock :: Eff (Haskeline : es) a -> Eff es a
runHaskelineMock = interpret_ $ \case
  GetInputLine _ -> pure $ Just "user input"

--------------------------------------------------------------------------------
--                                 Core logic                                 --
--------------------------------------------------------------------------------

runApp :: (Haskeline :> es, Logger :> es) => Eff es ()
runApp = do
  name <-
    getInputLine "Enter your name: " >>= \case
      Nothing -> error "Needed a name!"
      Just n -> pure n
  printStr $ "Effectful dynamic: " ++ name

--------------------------------------------------------------------------------
--                               Other effects                                --
--------------------------------------------------------------------------------

type instance DispatchOf Logger = Dynamic

data Logger :: Effect where
  PrintStr :: String -> Logger m ()

runLogger ::
  ( IOE :> es
  ) =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = interpret_ $ \case
  PrintStr s -> liftIO $ putStrLn s

printStr ::
  ( Logger :> es
  ) =>
  String ->
  Eff es ()
printStr = send . PrintStr

--------------------------------------------------------------------------------
--                                    Main                                    --
--------------------------------------------------------------------------------

runIO :: IO ()
runIO = H.runInputT H.defaultSettings $ HR.fromReaderT $ ReaderT $ \env -> do
  runEff
    . runReader env
    . runLogger
    . runHaskeline
    $ runApp
