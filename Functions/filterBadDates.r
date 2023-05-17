# Updated Feb 2022 for the purpose of running CMMN trend analysis by Danielle Ethier
# BadDates file was saved locally and not being read directly from the database. 


# this function filters records from a BMDE dataframe based on a set of exclusion filters stored
# in a data table. Each record filter represents a set of conditions used to remove records from the BMDE
# table. Specific conditions are not used when equal to 0. For example, a filter record where survey_year = 2005
# (with all other filters set to 0) would result in the removal of all records from 2005. Filters can be as specific
# as needed, and are based on the following BMDE fields: project_id, SiteCode, species_id, survey_year, survey_month,
# and survey_day. A field named period can also be used, as long as it is present in the dataframe. Filters based on
# fields that are not present in the dataframe will generate errors.

# Parameters:
# df: dataframe to filter. Must contain at least all fields that have non-zero values in the filters used, among
#     the list of following possible fields: project_id, SiteCode, species_id, survey_year, survey_month,
#     survey_day and period.
# verbose: boolean. Whether details of each step should be printed.
# ...: optional parameters used to restrict which filters are used. Default is to use all date ranges available.
#      See other parameters in bscdata.readBMDEFilterDateRanges.

bscdata.filterBadDates <- function(df, verbose=FALSE, ...) {

# if (verbose) print(paste("dataframe size before transformation:", NROW(df), "records"))

  # exclude filtered records (based on date, project, sitecode and species when applicable)
  # any filter with a value > 0 will be used to exclude data from the bmde dataframe

  #baddates.df <- bscdata.readBMDEFilterBadDates(verbose=verbose, ...)
  
  baddates.df<-read.csv("Data/bad_dates.csv") #this was changed to read the bad dates from the local file. 
  baddates.df$start_year[baddates.df$start_year == 0] <- NA
  baddates.df$end_year[baddates.df$end_year == 0] <- NA
  baddates.df$period[is.na(baddates.df$period)] <- 0
  
  
  # convert the filter fields names to match BMDE. Should probably rename the table fields instead!
  #names(baddates.df) <- c("project_id","SurveyAreaIdentifier","species_id","YearCollected","MonthCollected","DayCollected","period", "comment", "start_year", "end_year")
  baddates.df<-baddates.df %>% dplyr::rename(SurveyAreaIdentifier = SiteCode, YearCollected=survey_year, MonthCollected=survey_month, DayCollected=survey_day)
  
  #Added filter 2022 to accommodate the new format 
  
  if(length(site.list)>1){ #this is only true for LPBO
    baddates.df<-baddates.df %>% filter(SurveyAreaIdentifier %in% c("LPBO1", "LPBO2", "LPBO3"))  
  }else{
    baddates.df<-baddates.df %>% filter(SurveyAreaIdentifier==site.list)  
  }
  
  if(length(baddates.df$project_id) > 0) {
    baddates.df$filtered <- 1
  }

# ffields are fields used for filtering
# ffields <- c("project_id","SiteCode","species_id","survey_year","survey_month","survey_day","period")
# ffields <- c("ProjectCode","SurveyAreaIdentifier","species_id","YearCollected","MonthCollected","DayCollected","period")
  ffields <- c("SurveyAreaIdentifier","species_id","YearCollected","MonthCollected","DayCollected")
  
  # bfields are fields in the dataframe. This is used to subset the modify dataframe back to its original columns
  bfields <- names(df)


  # loops through individual filters to create a merge record, and any record found in bmde is removed
 
   if(NROW(baddates.df) > 0) {

    for (i in 1:NROW(baddates.df)) {
       byfields <- c()
       suppressWarnings(
         for (j in 1:length(ffields)) {
           if (!is.numeric(baddates.df[i, ffields[j]]) | (is.numeric(baddates.df[i, ffields[j]]) & baddates.df[i, ffields[j]] != 0)) byfields <- c(byfields,ffields[j])
         }
       )
       df <- merge(df, baddates.df[i,], by=byfields, suffixes=c("",".y"), all.x=T)

       df <- subset(df, is.na(filtered) | YearCollected < ifelse(is.na(start_year),0,start_year) | YearCollected > ifelse(is.na(end_year),9999,end_year), select=bfields)

       if (verbose) {
         print(baddates.df[i,])
         print(paste("dataframe size after step", i, ":", NROW(df), "records"))
         # print(paste("step ", as.character(i), ":", sum(!is.na(df[,"filtered"])), "records excluded"))
       }

    }
  }
#  if (verbose) print(paste("dataframe size after transformation:", ifelse(is.na(df),0,NROW(df)), "records"))

  return (df)

}

