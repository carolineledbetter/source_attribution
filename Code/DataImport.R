library(DBI)
library(odbc)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb)};Dbq=DataRaw/NORS9814.mdb;")
AllTables <- dbListTables(con)
AllTables <- AllTables[-grep('^MSys', AllTables)]
sapply(AllTables, function(x){
  assign(x, dbReadTable(con, x), .GlobalEnv)
  return(x)
  })

save(list = AllTables, file = 'DataRaw/NORS1998_2014.RData')
