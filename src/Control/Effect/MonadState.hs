{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wno-orphans #-}
module Control.Effect.MonadState
 (StateT
 ,evalStateT
 ,execStateT
 ,runStateT
 )where

import Control.Effect
import Control.Effect.Interpose
import Control.Effect.Interpret
import Control.Effect.State as E
import Control.Monad.State.Class as MTL (MonadState(..))
import Data.Tuple

type StateT = StateC

evalStateT :: Functor m => StateC s m a -> s -> m a
evalStateT = flip E.evalState

execStateT :: Functor m => StateC s m a -> s -> m s
execStateT = flip E.execState

runStateT :: Functor m => StateC s m a -> s -> m (a,s)
runStateT s = fmap swap . flip E.runState s

instance (Carrier sig m, Effect sig, Monad m) => MonadState s (StateC s m) where
  get = E.get
  put = E.put

#define PASSTHROUGH(head)\
instance (MonadState s m, Monad m) => MonadState s (head) where\
  state = MTL.state

PASSTHROUGH(CutC      m)
PASSTHROUGH(CullC     m)
PASSTHROUGH(ErrorC  e m)
PASSTHROUGH(FailC     m)
PASSTHROUGH(FreshC    m)
PASSTHROUGH(InterposeC eff m)
PASSTHROUGH(InterpretC eff m)
PASSTHROUGH(InterpretStateC eff s m)
PASSTHROUGH(LiftC     m)
PASSTHROUGH(NonDetC   m)
PASSTHROUGH(OnceC     m)
PASSTHROUGH(RandomC a m)
PASSTHROUGH(ReaderC w m)
PASSTHROUGH(ResourceC m)
PASSTHROUGH(ResumableC a m)
PASSTHROUGH(ResumableWithC a m)
PASSTHROUGH(TraceByIgnoringC  m)
PASSTHROUGH(TraceByPrintingC  m)
PASSTHROUGH(TraceByReturningC m)
PASSTHROUGH(WriterC w m)