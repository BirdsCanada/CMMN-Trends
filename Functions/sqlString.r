# returns an object value in a SQL-friendly format for insert

bscdata.sqlString <- function(obj, nastring="NULL") {

   if (is.na(obj)) return(nastring)

   if (class(obj) == "numeric") return(as.character(obj[1]))

   else return(paste("'", gsub("(')", "''", obj[1]), "'", sep=""))

}
