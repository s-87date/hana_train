# csvにしてからmysqlにアップロードしたほうが早い
library(RMySQL)
library(logging)

dbconnector <- dbConnect(MySQL(), 
                         dbname = "hoge_dev",
                         user="hogehoge", 
                         password="hogehoge",
                         host="hogehogehoge",
                         port=hoge)

dbGetQuery(dbconnector, "set character_set_client = utf8;")
dbGetQuery(dbconnector, "set character_set_connection = utf8;")
dbGetQuery(dbconnector, "set character_set_database = utf8;")
dbGetQuery(dbconnector, "set character_set_results = utf8;")
dbGetQuery(dbconnector, "set character_set_server = utf8;")

table.loader <- function(object, tmp.csv, table.name, mode){
  
  #Export csv
  export.temp.file <- tmp.csv
  write.csv(object, export.temp.file, quote=TRUE, row.names=FALSE, fileEncoding="UTF-8")
  
  insert.query <- paste("LOAD DATA LOCAL INFILE'", export.temp.file , "' INTO TABLE ", table.name, " FIELDS TERMINATED BY ',' ENCLOSED BY '\"' IGNORE 1 LINES;", sep = "")
  
  if (mode == 1){
    truncate.query <- paste("TRUNCATE TABLE ", table.name, sep = "")
    loginfo(paste(table.name,"テーブルをTRUNCATEします。", sep = ""))
    dbGetQuery(dbconnector, truncate.query)
  }
  
  loginfo(paste(table.name,"テーブルにデータを取り込みます。", sep = ""))
  dbGetQuery(dbconnector, insert.query)
  loginfo(paste(table.name,"テーブルへの取り込みが完了しました。", sep = ""))
}
