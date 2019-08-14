{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wno-orphans #-}
module Control.Effect.MonadWriter
  ( WriterT
  , runWriterT
  ) where

import Control.Effect
import Control.Effect.Interpose
import Control.Effect.Interpret
import Control.Effect.Writer as E
import Control.Monad.Trans
import Control.Monad.Writer.Class as MTL (MonadWriter(..))
import Data.Tuple

type WriterT = WriterC

runWriterT :: (Functor m, Monoid w) => WriterC w m a -> m (a,w)
runWriterT = fmap swap . E.runWriter

instance (Monoid w, Carrier sig m, Effect sig, Monad m) => MonadWriter w (WriterC w m) where
  tell = E.tell
  listen = fmap swap . E.listen
  pass action = do
    (w,(res, f)) <- lift $ runWriter action
    E.tell (f w)
    return res

#define PASSTHROUGH(head)\
instance (MonadWriter w m, Monad m) => MonadWriter w (head) where\
  tell = MTL.tell;\
  listen = MTL.listen;\
  pass = MTL.pass

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
PASSTHROUGH(StateC  w m)
PASSTHROUGH(TraceByIgnoringC  m)
PASSTHROUGH(TraceByPrintingC  m)
PASSTHROUGH(TraceByReturningC m)