
module MysqlUpdate where

import Database.HaskellDB.Query
import Database.HaskellDB.HSQL.MySQL

import Database.HaskellDB.DBSpec.DatabaseToDBSpec
import Database.HaskellDB.DBSpec.DBSpecToDBDirect

import FinanceDatabaseSpec
import FinanceDatabaseSpec.Quotes as Quotes
import FinanceDatabaseSpec.Stocks as Stocks

dbOptions = [("server","zaphod"),("uid","peter"),("pwd","al"),("db","finance")]

withDB f = connect driver dbOptions f



regenSpec = do
  dbi <- withDB (dbToDBSpec True "finance")
  dbInfoToModuleFiles "." "FinanceDatabaseSpec.hs" dbi

