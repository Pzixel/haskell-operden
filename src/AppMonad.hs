{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module AppMonad where

import Control.Monad.Catch     (MonadCatch, MonadThrow)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger    (NoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader    (MonadReader, ReaderT(ReaderT), asks, runReaderT)
import Control.Monad.Trans     (lift)

import Conduit
import Data.Time
import Database.Persist.Postgresql
import Database.Persist.TH

type App = AppT IO

data Env =
  Env
    { connStr :: ConnectionString
    , port    :: Int
    }
  deriving (Eq, Show)

newtype AppT m a =
  AppT (ReaderT Env m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Env)

deriving instance MonadTrans AppT

deriving instance MonadUnliftIO App

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Player
    name String
    email String
    deriving Show
Item
    symbol String
    amount Int
    price Double
    idPlayer PlayerId
    ts UTCTime
    deriving Show
|]

type QueryT b a = ReaderT b (NoLoggingT (ResourceT IO)) a

withDb ::
     forall m b a. (MonadUnliftIO m, IsPersistBackend b, BaseBackend b ~ SqlBackend)
  => QueryT b a
  -> AppT m a
withDb action = do
  cs <- asks connStr
  lift $ runStdoutLoggingT $ withPostgresqlPool cs 1 $ liftSqlPersistMPool action

example :: App ()
example =
  withDb $ do
    runMigration migrateAll
    idPlayer <- insert $ Player "Test" "test@test.com"
    time <- liftIO getCurrentTime
    idItem1 <- insert $ Item "CASH" 10000 1.0 idPlayer time
    idItem2 <- insert $ Item "PAH3.DE" 0 0 idPlayer time
    liftIO $ putStrLn $ "keys = " <> show idItem1 <> ", " <> show idItem2

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

testMain :: IO ()
testMain = do
  let env = Env
        { connStr = "host=localhost dbname=operdenstorage user=pguser password=mycoolpass port=5432"
        , port = 8080
        }
  runAppT env example
