# saves a dataframe to an existing ODBC table. The field names are taken from the dataframe column names, unless specified
# you must ensure that these names are exactly the same (but not case-sensitive). IMPORTANT: You need to be aware that in
# the current version, failed inserts do not generate any errors or warnings.

# tablename:  name of the destination ODBC table
# df:         dataframe containing the data to save
# fields:     vector of field names of the ODBC table. If not specified, uses the column names of the dataframe.
# textfile:   optional. full path of a text file where the SQL commands for batch insert are saved. this option allows
#             to generate a batch upload file (as an alternative or in option to saving the records in the ODBC table)
# append:     boolean. whether batch commands should be appended to an existing textfile (textfile must be provided)
# savetable:  boolean. whether the SQL INSERT commands are executed immediately. Insert permissions are required for
#             your personal database login associated with your ODBC connection.
# datasource: name of ODBC datasource used to create connection to database (required when savetable is TRUE)

# example: bscdata.sqlSave("results_annual_indices", df, textfile="c:/temp1.sql")
# example: bscdata.sqlSave("results_trends", df, textfile="c:/temp1.sql", savetable=F, datasource="dendroica")

bscdata.sqlSave <- function(tablename, df, fields=names(df), textfile=NA, append=T, savetable=F, datasource=NA) {

  fieldnames <- ""
  for (i in 1:length(fields)) {
    fieldnames <- paste(fieldnames, "[", fields[i], "]", ifelse(i ==length(fields), "", ", "), sep="")
  }

  if (savetable) {
     if (is.na(datasource)) {
       warning("the ODBC data source was not specified. the table will not be saved.")
       savetable = F
     } else {
       channel <- odbcConnect(datasource)
     }
  }
  if (!append & !is.na(textfile)) write("", textfile, append=F)

  errorcnt <- 0
  for (i in 1:length(df[,1])) {

     sql1 <- paste("INSERT INTO ", tablename, " (", fieldnames, ") VALUES (", sep="")

     for (j in 1:length(df[i,])) {
        sql1 <- paste(sql1, ifelse(j > 1, ", ", ""),
          bscdata.sqlString(df[i,j]),
          ifelse(j == length(df[i,]), ")", ""), sep="")
     }

     if (savetable) {
        i <- sqlQuery(channel, sql1)
        if (length(i) > 0) {
          errorcnt <- errorcnt + 1
          if (errorcnt <= 50) warning(i)
        }
     }

     if (!is.na(textfile)) write(sql1, textfile, append=T)

  }
  if (savetable) {
   odbcClose(channel)
   rm(channel)
  }
  if (errorcnt > 0) {
     print(paste("Some records failed to save: ", as.character(errorcnt), " out of a total of ", as.character(length(df[,1])), sep=""))
  }
}

