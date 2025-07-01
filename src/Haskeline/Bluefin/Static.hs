module Haskeline.Bluefin.Static
  ( -- * IO
    runIO,

    -- * Effect
    Haskeline (..),
    getInputLine,

    -- ** Handlers
    runHaskeline,
  )
where

import Bluefin.Compound (Handle (mapHandle), useImplIn)
import Bluefin.Eff (Eff, Effects, runEff_, type (:&), type (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.Reader (Reader, ask, runReader)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Data.Kind (Type)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

--------------------------------------------------------------------------------
--                               Static Effect                                --
--------------------------------------------------------------------------------

type Haskeline :: Effects -> Type
newtype Haskeline e = MkHaskeline (IOE e)

-- | @since 0.1
instance Handle Haskeline where
  mapHandle (MkHaskeline io) = MkHaskeline (mapHandle io)

runHaskeline ::
  forall hEff es r.
  (hEff :> es) =>
  IOE hEff ->
  (forall e. Haskeline e -> Eff (e :& es) r) ->
  Eff es r
runHaskeline ioe k = useImplIn k (MkHaskeline $ mapHandle ioe)

getInputLine ::
  ( e1 :> es,
    e2 :> es
  ) =>
  Reader (InputTEnv IO) e1 ->
  Haskeline e2 ->
  String ->
  Eff es (Maybe String)
getInputLine rdr (MkHaskeline ioe) = liftInputT ioe rdr . H.getInputLine

liftInputT ::
  ( e1 :> es,
    e2 :> es
  ) =>
  IOE e1 ->
  (Reader (InputTEnv IO) e2) ->
  InputT IO a ->
  Eff es a
liftInputT io rdr f = ask rdr >>= effIO io . runReaderT (HR.toReaderT f)

--------------------------------------------------------------------------------
--                                 Core logic                                 --
--------------------------------------------------------------------------------

runApp ::
  ( e1 :> es,
    e2 :> es,
    e3 :> es
  ) =>
  Reader (InputTEnv IO) e1 ->
  Logger e2 ->
  Haskeline e3 ->
  Eff es ()
runApp rdr l h = do
  name <-
    getInputLine rdr h "Enter your name: " >>= \case
      Nothing -> error "Needed a name!"
      Just n -> pure n
  printStr l $ "Bluefin static: " ++ name

--------------------------------------------------------------------------------
--                               Other effects                                --
--------------------------------------------------------------------------------

type Logger :: Effects -> Type
newtype Logger e = MkLogger (IOE e)

instance Handle Logger where
  mapHandle (MkLogger io) = MkLogger (mapHandle io)

runLogger ::
  forall lEff es r.
  (lEff :> es) =>
  IOE lEff ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
runLogger ioe k = useImplIn k (MkLogger $ mapHandle ioe)

printStr ::
  ( e :> es
  ) =>
  Logger e ->
  String ->
  Eff es ()
printStr (MkLogger ioe) = effIO ioe . putStrLn

--------------------------------------------------------------------------------
--                                    Main                                    --
--------------------------------------------------------------------------------

runIO :: IO ()
runIO = H.runInputT H.defaultSettings $ HR.fromReaderT $ ReaderT $ \env -> do
  runEff_ $ \io -> do
    runReader env $ \rdr -> do
      runLogger io $ \logger -> do
        runHaskeline io $ \h -> runApp rdr logger h
