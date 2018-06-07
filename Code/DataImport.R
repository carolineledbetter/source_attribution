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

con2 <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb)};Dbq=DataRaw/WHIT_20180502_NoWater.mdb;")
AllTables_NoWater <- dbListTables(con2)
AllTables_NoWater<- AllTables_NoWater[-grep('^MSys|^~T', AllTables_NoWater)]
sapply(AllTables_NoWater, function(x){
  assign(x, dbReadTable(con2, x), .GlobalEnv)
  return(x)
})
save(list = AllTables_NoWater, file = 'DataRaw/WHIT_20180502_NoWater.RData')
