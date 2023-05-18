#source("00_setup.R")

  collection <- as.character(anal.param[t, "collection"])
  station <- as.character(anal.param[t, "station"])
  site <- as.character(anal.param[t, "site"])
  site.specific <- anal.param[t, "site.specific"]
  min.species <- anal.param[t, "min.species"]
  use.trfl <- anal.param[t, "use.trfl"]
  responseM<-anal.param[t , "obs.var.M"]
  responseO<-anal.param[t , "obs.var.O"]
    
## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. First, it will look to see if you have a copy saved in the data directory. 

in.data <-try(read.csv(paste(data.dir, site, "_Raw_Data.csv", sep="")))

if(class(in.data) == 'try-error'){

in.data <- nc_data_dl(collections = collection, fields_set = "extended", username = ID, info="CMMN Superfile Scripts", years=c(min.year, max.year))

} #end of try catch, which looks for the raw data in the data.dir first

in.data<- in.data %>% filter(YearCollected >= min.year & YearCollected <= max.year)
in.data <- in.data %>% select(SurveyAreaIdentifier, project_id, ObservationCount, ObservationCount2, ObservationCount3, ObservationCount4, SiteCode, YearCollected, MonthCollected, DayCollected, species_id, SpeciesCode)

# we only want sites LPBO1, LPBO2, and LPBO3. 
if(station == "LPBO") {
  in.data <- in.data %>%
    distinct() %>%
    filter(SurveyAreaIdentifier != "LPBO-03") %>% 
    droplevels()
}

# This is a fix while Catherine is away - there are two names for this site VLMMS 2001-2015, and VLBO 2016-2017 which I believe should be same site, so rename
if((station == "VLBO")) {
  in.data <- in.data %>%
    mutate(SurveyAreaIdentifier = "VLBO") %>%
    droplevels()
}

# McGill is in the database as both MGBO and MBO
if((station == "MGBO")) {
  in.data <- in.data %>%
    mutate(SurveyAreaIdentifier = "MGBO") %>%
    droplevels()
}

#  if RPBO only want site RPBO (not PEBA or RBPO2)
if((station == "RPBO")) {
  in.data <- in.data %>%
    filter(SurveyAreaIdentifier == "RPBO") %>%
    droplevels()
}

## Generate site list - this will have a length of 1 for most sites; at LPBO will include all three sites
site.list <- as.character(unique(in.data$SurveyAreaIdentifier))

# DROP BAD DATES:drop days that should be excluded from a site for one reason or another.  We do this again later to drop species-specific bad dates.
# Note: I made some manual tweaks to the function. Need to check that it works for all sites. 
in.data <- bscdata.filterBadDates(in.data, sitecode = site.list)

## Assign date and season variables
in.data <- in.data %>%
  mutate(date = ymd(paste(YearCollected, MonthCollected, DayCollected, sep = "/")),
         doy = yday(date),
         season = if_else(doy < 180, "Spring", "Fall"))


# get the minimum number of years a species must be detected to be included. Could be MUCH more conservative... currently using 1/2 of years surveyed, but those species might also get kicked out by abundance filters below.
min.yrs.detect <- trunc(length(unique(in.data$YearCollected))/2) 


#create events data for zero filling
event.data <- in.data %>%
  filter(ObservationCount > 0) %>%
  group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season) %>%
  mutate(nspecies = n()) %>%
  select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season) %>% 
  distinct() %>%
  ungroup() %>%
  as.data.frame()

## Assign Species Code, and drop species that won't be analyzed

## re-assign species codes

in.data %>% filter(SpeciesCode %in% c("SOVI", "EATO", "NSTS", "TRFL", "WEFL", "WISN", "NOFL", "WPWI", "PAWA","DEJU","YRWA")) %>%
  group_by(SpeciesCode) %>%
  tally()

in.data <- in.data %>%
  mutate(SpeciesCode = as.character(SpeciesCode),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "BHVI"|SpeciesCode == "CAVI"), "SOVI"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "YEWA"), "YWAR"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "RSTO"|SpeciesCode == "URST"), "EATO"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "GBTH"|SpeciesCode == "GCBT"|SpeciesCode == "BITH"), "GCTH"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "NESP"|SpeciesCode == "STSP"), "NSTS"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "ALFL"|SpeciesCode == "WIFL"), "TRFL"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "GWCS"|SpeciesCode == "EWCS"), "WCSP"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "AGWT"), "GWTE"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "PSFL"), "WEFL"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "COSN"), "WISN"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "YSFL"|SpeciesCode == "FLIN"), "NOFL"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "EWPW"|SpeciesCode == "EWWP"), "WPWI"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "WPWA"|SpeciesCode == "UNPW"|SpeciesCode == "YPWA"), "PAWA"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "UYRW"|SpeciesCode == "MYWA"|SpeciesCode == "AUWA"), "YRWA"),
         SpeciesCode = replace(SpeciesCode, (SpeciesCode == "SCJU"|SpeciesCode == "GHJU"|SpeciesCode == "ORJU"|SpeciesCode == "UDEJ"|SpeciesCode == "WWJU"|SpeciesCode == "YDEJ"|SpeciesCode == "PSJU"), "DEJU"))

in.data %>% filter(SpeciesCode %in% c("SOVI", "EATO", "NSTS", "TRFL", "WEFL", "WISN", "NOFL", "WPWI", "PAWA","DEJU","YRWA")) %>%
  group_by(SpeciesCode) %>%
  tally()


#total number of years each doy surveyed at each site (include 0-obs counts)
df.totYears<-NULL
df.totYears <- in.data %>%
  select(SurveyAreaIdentifier, YearCollected, doy) %>%
  distinct() %>%
  group_by(SurveyAreaIdentifier, doy) %>%
  summarize(totYears = n()) %>%
  as.data.frame()%>% 
  mutate(prop_year= totYears/((max.year-min.year)+1)) %>% 
  mutate(season = if_else(doy < 180, "Spring", "Fall"))

write.csv(df.totYears, paste(data.dir, site, "_YearsSurvey.csv", sep=""), row.names = FALSE)


## get total count by species, date, station
in.data$ObservationCount<-as.numeric(in.data$ObservationCount)
in.data$ObservationCount2<-as.numeric(in.data$ObservationCount2)
in.data$ObservationCount3<-as.numeric(in.data$ObservationCount3)
in.data$ObservationCount4<-as.numeric(in.data$ObservationCount4)

in.data <- in.data %>%
  group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, SpeciesCode, species_id, date, doy, season) %>%
  dplyr::summarize(ObservationCount = sum(ObservationCount, na.rm = TRUE),
                   ObservationCount2 = sum(ObservationCount2, na.rm = TRUE),
                   ObservationCount3 = sum(ObservationCount3, na.rm = TRUE),
                   ObservationCount4 = sum(ObservationCount4, na.rm = TRUE)) %>%
as.data.frame()

## create new response variable for census + band
in.data$ObservationCount7=in.data$ObservationCount3+in.data$ObservationCount4
## add the start and end dates for the 95% station survey window.
#in.data <- left_join(in.data, df.sampleDates, by = c("SurveyAreaIdentifier", "season")) 

## Run 'Assign UNEM.R' For LPBO data only!!!  
#This section modifies empidonax values by assigning unknown empidonax to  LEFL, YBFL, TRFL by the proportional representation of each species on each doy (across years) in ET data. Might use banding data (ObservationCount4), but not available digitally pre-1984. NOTE that by running this, banding data and census data are erased from in.data for empid species. Do not include ACFL because there as so few it would make little impact.

if(site == "LPBO") {
  
## 1. summarize the total of known empidonax species by doy/site, across years
  
  df.empidSum <- in.data %>%
    filter(SpeciesCode %in% c("LEFL", "YBFL", "TRFL")) %>%
    group_by(SurveyAreaIdentifier, doy) %>%
    summarize(KnownEmpidET = sum(ObservationCount, na.rm = TRUE),
              KnownEmpidCensus = sum(ObservationCount4, na.rm = TRUE))
  
## 2. summarize total of each empid species by doy/site, across years
  
  df.empidSum2 <- in.data %>%
    filter(SpeciesCode %in% c("LEFL", "YBFL", "TRFL")) %>%
    group_by(SurveyAreaIdentifier, doy, SpeciesCode) %>%
    summarize(totET = sum(ObservationCount, na.rm = TRUE),
              totCensus = sum(ObservationCount4, na.rm = TRUE))
  
##3. merge to get proportion of total empid for each species by site, doy
  
  df.empidSum <- left_join(df.empidSum, df.empidSum2, by = c("SurveyAreaIdentifier", "doy")) %>%
    mutate(p.totET = totET/KnownEmpidET,
           p.totCensus = totCensus/KnownEmpidCensus) %>%
    select(SurveyAreaIdentifier, doy, SpeciesCode, p.totET, p.totCensus)
  
  #ggplot(df.empidSum, aes(y = p.tot, x = doy, colour = SpeciesCode)) + geom_point()
  
##4. merge this with total number of unknown empidonax
  
  df.unem <- in.data %>%
    filter(SpeciesCode %in% c("UNEM")) %>%
    group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy) %>%
    summarize(totUNEMET = sum(ObservationCount, na.rm = TRUE),
              totUNEMCensus = sum(ObservationCount4, na.rm = TRUE)) %>%
    left_join(df.empidSum, by = c("SurveyAreaIdentifier","doy")) %>%
    mutate(addObsET = round(totUNEMET*p.totET, digits = 0),
           addObsCensus = round(totUNEMCensus*p.totCensus, digits = 0)) %>%
    as.data.frame() %>%
    select(-totUNEMET, -totUNEMCensus,-p.totET, -p.totCensus)
  
##5. merge this with the raw data, and sum addObs and ObservationCount
  
  in.data <- left_join(in.data, df.unem, by = c("SurveyAreaIdentifier", "YearCollected", "MonthCollected", "DayCollected", "date", "doy", "SpeciesCode")) %>%
    mutate(addObsET = if_else(is.na(addObsET), 0, addObsET),
           addObsCensus = if_else(is.na(addObsCensus), 0, addObsCensus),
           ObservationCount = ObservationCount + addObsET,
           ObservationCount4 = ObservationCount4 + addObsCensus) %>%
    select(-addObsET, -addObsCensus) %>%
    filter(SpeciesCode != "UNEM")
  
  } #end if site = LPBO
    

#write clean data to file
write.csv(in.data, paste(data.dir, site, "_Raw_Superfile.csv", sep=""), row.names = FALSE)
write.csv(event.data, paste(data.dir, site, "_Event_Superfile.csv", sep=""), row.names = FALSE)

## Generate Species list for analysis
species.list <- unique(in.data$SpeciesCode)




