module Haskeline.Bluefin.Dynamic
  ( -- * IO
    runIO,

    -- * Effect
    Haskeline (..),
    getInputLine,

    -- ** Handlers
    runHaskeline,
  )
where

import Bluefin.Compound (Handle (mapHandle), makeOp, useImplIn, useImplUnder)
import Bluefin.Eff (Eff, Effects, runEff_, type (:&), type (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.Reader (Reader, runReader)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Kind (Type)
import Haskeline.Bluefin.Static qualified as Static
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

--------------------------------------------------------------------------------
--                               Static Effect                                --
--------------------------------------------------------------------------------

type Haskeline :: Effects -> Type
data Haskeline es = MkHaskeline
  { getInputLineImpl ::
      forall e.
      String ->
      Eff (e :& es) (Maybe String)
  }

-- | @since 0.1
instance Handle Haskeline where
  mapHandle e =
    MkHaskeline
      { getInputLineImpl = useImplUnder . getInputLineImpl e
      }

runHaskeline ::
  forall e1 e2 es r.
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Reader (InputTEnv IO) e2 ->
  (forall e. Haskeline e -> Eff (e :& es) r) ->
  Eff es r
runHaskeline ioe rdr k =
  useImplIn
    k
    $ MkHaskeline
      { getInputLineImpl = Static.getInputLine rdr e
      }
  where
    e = Static.MkHaskeline ioe

getInputLine ::
  ( e :> es
  ) =>
  Haskeline e ->
  String ->
  Eff es (Maybe String)
getInputLine e = makeOp . getInputLineImpl (mapHandle e)

--------------------------------------------------------------------------------
--                                 Core logic                                 --
--------------------------------------------------------------------------------

runApp ::
  ( e1 :> es,
    e3 :> es
  ) =>
  Logger e1 ->
  Haskeline e3 ->
  Eff es ()
runApp l h = do
  name <-
    getInputLine h "Enter your name: " >>= \case
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
        runHaskeline io rdr $ \h -> runApp logger h
