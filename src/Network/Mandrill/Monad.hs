{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mandrill.Monad where

import Network.Mandrill.Types
import Control.Applicative
import Control.Monad.Reader


newtype MandrillT m a = MandrillT {
    runMandrillT :: ReaderT ApiKey m a
  } deriving ( MonadTrans, MonadReader ApiKey
             , Functor, Applicative, Monad, MonadIO)


runMandrill :: MonadIO m => ApiKey -> MandrillT m a -> m a
runMandrill k action = runReaderT (runMandrillT action) k

