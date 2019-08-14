{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wno-orphans #-}
module Control.Effect.MonadControl where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.Lift
import           Control.Effect.Interpose
import           Control.Effect.Interpret
import           Control.Effect.Random
import           Control.Effect.Reader
import           Control.Effect.Resumable
import           Control.Effect.State
import           Control.Effect.Trace
import           Control.Effect.Writer
import           Control.Monad.Base
import           Control.Monad.Trans
import           Control.Monad.Trans.Control

#define MONADBASE(head)\
instance MonadBase b m => MonadBase b (head) where\
  liftBase = lift . liftBase

MONADBASE(CutC      m)
MONADBASE(CullC     m)
MONADBASE(ErrorC  e m)
MONADBASE(FailC     m)
MONADBASE(FreshC    m)
MONADBASE(InterposeC eff m)
MONADBASE(InterpretC eff m)
MONADBASE(InterpretStateC eff s m)
MONADBASE(LiftC     m)
MONADBASE(NonDetC   m)
MONADBASE(RandomC a m)
MONADBASE(ReaderC w m)
MONADBASE(ResourceC m)
MONADBASE(ResumableC a m)
MONADBASE(ResumableWithC a m)
MONADBASE(StateC  w m)
MONADBASE(TraceByIgnoringC  m)
MONADBASE(TraceByPrintingC  m)
MONADBASE(TraceByReturningC m)
MONADBASE(WriterC         w m)

#define MONADBASECONTROL(T)\
instance MonadBaseControl b m => MonadBaseControl b (T m) where\
  type StM (T m) a = ComposeSt (T) m a;\
  liftBaseWith = defaultLiftBaseWith;\
  restoreM = defaultRestoreM

MONADBASECONTROL(ErrorC  e)
MONADBASECONTROL(FailC    )
MONADBASECONTROL(FreshC   )
MONADBASECONTROL(LiftC    )
MONADBASECONTROL(RandomC r)
MONADBASECONTROL(ReaderC w)
MONADBASECONTROL(StateC  w)
MONADBASECONTROL(TraceByIgnoringC )
MONADBASECONTROL(TraceByPrintingC )
MONADBASECONTROL(TraceByReturningC)
MONADBASECONTROL(WriterC         w)

instance MonadTransControl (ErrorC e) where
  type StT (ErrorC e) a = Either e a
  liftWith f = ErrorC $ fmap return $ f $ runErrorC
  restoreT = ErrorC

instance MonadTransControl LiftC where
  type StT LiftC a = a
  liftWith f = LiftC $ f runLiftC
  restoreT = LiftC

instance MonadTransControl (ReaderC r) where
  type StT (ReaderC r) a = a
  liftWith f = ReaderC $ \r -> f $ \t -> runReaderC t r
  restoreT = ReaderC . const

instance MonadTransControl (StateC s) where
  type StT (StateC s) a = (s,a)
  liftWith f = StateC $ \s -> fmap (s,) $ f $ \t -> runState s t
  restoreT = StateC . const

instance MonadTransControl TraceByPrintingC where
  type StT TraceByPrintingC a = a
  liftWith f = TraceByPrintingC $ f runTraceByPrintingC
  restoreT = TraceByPrintingC

instance MonadTransControl TraceByIgnoringC where
  type StT TraceByIgnoringC a = a
  liftWith f = TraceByIgnoringC $ f runTraceByIgnoringC
  restoreT = TraceByIgnoringC

deriving instance MonadTransControl FailC
deriving instance MonadTransControl FreshC
deriving instance MonadTransControl (RandomC g)
deriving instance MonadTransControl (ResumableC err)
deriving instance MonadTransControl TraceByReturningC
deriving instance MonadTransControl (WriterC w)