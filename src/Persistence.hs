{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoDerivingStrategies       #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE PartialTypeSignatures           #-}
{-# LANGUAGE NamedWildCards           #-}

module Persistence where
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Logger    (runStderrLoggingT, NoLoggingT)
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time.Calendar
import Data.Time
import GHC.Generics hiding (from)
import Database.Esqueleto
import qualified Database.Persist as P
import Conduit

foo = id

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age Int
    email String
    registration_date Day
    deriving Eq Show Generic
|]

connStr :: ConnectionString
connStr = "host=localhost dbname=operdenstorage user=pguser password=mycoolpass port=5432"

getPersonsInner :: (MonadIO m) => SqlPersist m [User]
getPersonsInner = do
  people <- select $
                from $ \person -> do
                return person
  pure $ fmap entityVal people

runInDb :: IsSqlBackend backend => ReaderT backend (NoLoggingT (ResourceT IO)) a -> IO a
runInDb f = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ runSqlPersistMPool f pool

getPersons :: IO [User]
getPersons = runInDb getPersonsInner
