{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Haskeline.Effectful.Static
  ( -- * IO
    runIO,

    -- * Effect
    Haskeline,
    getInputLine,

    -- ** Handlers
    runHaskeline,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Effectful
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Effectful.Reader.Static (Reader, ask, runReader)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

--------------------------------------------------------------------------------
--                               Static Effect                                --
--------------------------------------------------------------------------------

data Haskeline :: Effect

type instance DispatchOf Haskeline = Static WithSideEffects

data instance StaticRep Haskeline = MkHaskeline

runHaskeline :: (IOE :> es) => Eff (Haskeline : es) a -> Eff es a
runHaskeline = evalStaticRep MkHaskeline

getInputLine ::
  ( Haskeline :> es,
    Reader (InputTEnv IO) :> es
  ) =>
  String ->
  Eff es (Maybe String)
getInputLine = liftInputT . H.getInputLine

liftInputT :: (Reader (InputTEnv IO) :> es) => InputT IO a -> Eff es a
liftInputT f = ask >>= unsafeEff_ . runReaderT (HR.toReaderT f)

--------------------------------------------------------------------------------
--                                 Core logic                                 --
--------------------------------------------------------------------------------

runApp :: (Haskeline :> es, Logger :> es, Reader (InputTEnv IO) :> es) => Eff es ()
runApp = do
  name <-
    getInputLine "Enter your name: " >>= \case
      Nothing -> error "Needed a name!"
      Just n -> pure n
  printStr $ "Effectful static: " ++ name

--------------------------------------------------------------------------------
--                               Other effects                                --
--------------------------------------------------------------------------------

data Logger :: Effect

type instance DispatchOf Logger = Static WithSideEffects

data instance StaticRep Logger = MkLogger

runLogger :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runLogger = evalStaticRep MkLogger

printStr ::
  ( Logger :> es
  ) =>
  String ->
  Eff es ()
printStr = unsafeEff_ . putStrLn

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
