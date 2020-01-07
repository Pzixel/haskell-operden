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

module Persistence where
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import Data.Time.Calendar
import Data.Time
import GHC.Generics

foo = id


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age Int
    email String
    registration_date Day
    deriving Eq Show Generic
|]

connStr = "host=localhost dbname=operdenstorage user=pguser password=mycoolpass port=5432"

m :: IO ()
m =
  runStderrLoggingT $
  withPostgresqlPool connStr 10 $ \pool ->
    liftIO $
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      liftIO $ putStrLn "Done"
