{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wno-orphans #-}
module Control.Effect.MonadReader
  ( ReaderT
  , runReaderT
  ) where

import           Control.Effect
import           Control.Effect.Interpose
import           Control.Effect.Interpret
import           Control.Effect.Reader      as E
import           Control.Monad.Reader.Class as MTL (MonadReader (..))

type ReaderT = ReaderC

runReaderT :: ReaderC r m a -> r -> m a
runReaderT = flip E.runReader

instance ( Carrier sig m, Monad m) => MonadReader r (ReaderC r m) where
  ask   = E.ask
  local = E.local

#define PASSTHROUGH(head)\
instance (MonadReader r m, Monad m) => MonadReader r (head) where\
  ask   = MTL.ask;\
  local = MTL.local

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
PASSTHROUGH(ResourceC m)
PASSTHROUGH(ResumableC a m)
PASSTHROUGH(ResumableWithC a m)
PASSTHROUGH(StateC  w m)
PASSTHROUGH(TraceByIgnoringC  m)
PASSTHROUGH(TraceByPrintingC  m)
PASSTHROUGH(TraceByReturningC m)
PASSTHROUGH(WriterC w m)
