# haskell-operden

## Prerequisites

Install PostgreSQL locally and run `createdb operdenstorage`

## Run

```bash
stack run
```

Expected output
```
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_haskell_operden
Linking .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/haskell-operden-exe/haskell-operden-exe ...

haskell-operden> copy/register
Installing library in /Users/...
Installing executable haskell-operden-exe in /Users/...
Registering library for haskell-operden-0.1.0.0..
Migrating: CREATe TABLE "player"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"email" VARCHAR NOT NULL)
[Debug#SQL] CREATe TABLE "player"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"email" VARCHAR NOT NULL); []
Migrating: CREATe TABLE "item"("id" SERIAL8  PRIMARY KEY UNIQUE,"symbol" VARCHAR NOT NULL,"amount" INT8 NOT NULL,"price" DOUBLE PRECISION NOT NULL,"id_player" INT8 NOT NULL,"ts" TIMESTAMP WITH TIME ZONE NOT NULL)
[Debug#SQL] CREATe TABLE "item"("id" SERIAL8  PRIMARY KEY UNIQUE,"symbol" VARCHAR NOT NULL,"amount" INT8 NOT NULL,"price" DOUBLE PRECISION NOT NULL,"id_player" INT8 NOT NULL,"ts" TIMESTAMP WITH TIME ZONE NOT NULL); []
Migrating: ALTER TABLE "item" ADD CONSTRAINT "item_id_player_fkey" FOREIGN KEY("id_player") REFERENCES "player"("id")
[Debug#SQL] ALTER TABLE "item" ADD CONSTRAINT "item_id_player_fkey" FOREIGN KEY("id_player") REFERENCES "player"("id"); []
[Debug#SQL] INSERT INTO "player"("name","email") VALUES(?,?) RETURNING "id"; [PersistText "Test",PersistText "test@test.com"]
[Debug#SQL] INSERT INTO "item"("symbol","amount","price","id_player","ts") VALUES(?,?,?,?,?) RETURNING "id"; [PersistText "CASH",PersistInt64 10000,PersistDouble 1.0,PersistInt64 1,PersistUTCTime 2020-01-16 23:08:44.318446 UTC]
[Debug#SQL] INSERT INTO "item"("symbol","amount","price","id_player","ts") VALUES(?,?,?,?,?) RETURNING "id"; [PersistText "PAH3.DE",PersistInt64 0,PersistDouble 0.0,PersistInt64 1,PersistUTCTime 2020-01-16 23:08:44.318446 UTC]
keys = ItemKey {unItemKey = SqlBackendKey {unSqlBackendKey = 1}}, ItemKey {unItemKey = SqlBackendKey {unSqlBackendKey = 2}}
```
