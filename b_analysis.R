source("00_setup.R")

# If the data exists, load, otherwise process in.data

mig.data<-NULL
mig.data <-try(read.csv(paste(out.dir, site, "_SuperData.csv", sep="")), silent =TRUE)

if(class(mig.data) == 'try-error'){

in.data <-read.csv(paste(out.dir, site, "_Raw_Superfile.csv", sep="")) 
  
#Create output table for site specific results
mig.data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 11, byrow = FALSE,
                                    dimnames = NULL))
names(mig.data) <- c("SurveyAreaIdentifier", "species_id", "species", "doy", "prop_years", "n_years", "season", "mean_Obs", "mean_Obs3", "mean_Obs4", "mean_Obs7")  
write.table(mig.data, file = paste(out.dir, 
                                      site, "_SuperData.csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#species loop
for(k in 1:length(species.list)) {
  
  sp.data<-NULL
  sp.data <- filter(in.data, SpeciesCode == species.list[k]) 
  species_id <- unique(sp.data$species_id)
  species <- species.list[k]
  
#zero-fill by merging event and real data.  
  
  sp.data <- left_join(event.data, sp.data, by = c("SurveyAreaIdentifier",  "YearCollected", "MonthCollected", "DayCollected", "date", "doy", "season")) %>%
    mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0),
    ObservationCount2 = replace(ObservationCount2, is.na(ObservationCount2), 0),
    ObservationCount3 = replace(ObservationCount3, is.na(ObservationCount3), 0),
    ObservationCount4 = replace(ObservationCount4, is.na(ObservationCount4), 0), 
    ObservationCount7 = replace(ObservationCount7, is.na(ObservationCount7), 0))
  
#Filter species that don't meet the abundance requirement
  
# Want to use 0-observation counts to get mean count across years, but drop 0-observation counts to get number of non-0 observation days, so do this in separate steps:
  
# 1. Drop seasons where mean number of individuals/year is < 10
  
  df.mean <- NULL
  df.mean <- sp.data %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(count = sum(ObservationCount)) %>% 
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanCount = mean(count, na.rm = TRUE)) %>%
    filter(meanCount >= 10)%>%
    as.data.frame()
  
  sp.data <- left_join(df.mean, sp.data, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
    select(-meanCount)%>%
    as.data.frame()
  
# 2. Drop seasons where mean number of observation days/year is < 5
  
  df.obs <- NULL
  df.obs <- sp.data %>%
    filter(ObservationCount > 0) %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(nobs = n()) %>%
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanObsDays = mean(nobs, na.rm = TRUE)) %>%
    filter(meanObsDays >= 5) %>%
    as.data.frame()
  
  sp.data <- left_join(df.obs, sp.data, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
    select(-meanObsDays) %>%
    as.data.frame()
  
  if(nrow(sp.data) > 0) {
    
    site.list <- as.character(unique(sp.data$SurveyAreaIdentifier))
      
    df.abund<-NULL
      df.abund <- sp.data %>%
      group_by(SurveyAreaIdentifier, doy, season) %>%
      summarize(mean_Obs = mean(ObservationCount, na.rm = TRUE), 
                mean_Obs3 = mean(ObservationCount3, na.rm = TRUE), 
                mean_Obs4 = mean(ObservationCount4, na.rm = TRUE), 
                mean_Obs7 = mean(ObservationCount7, na.rm = TRUE))
 
    df.nyears<-NULL
       df.nyears <- sp.data %>%
        filter(ObservationCount > 0) %>%
        select(SurveyAreaIdentifier, YearCollected, doy) %>%
        distinct() %>%
        group_by(SurveyAreaIdentifier, doy) %>%
        dplyr::summarize(nyears = n()) %>%
        as.data.frame()
      
    df.totYears<-NULL
      df.totYears <- event.data %>%
        select(SurveyAreaIdentifier, YearCollected, doy) %>%
        distinct() %>%
        group_by(SurveyAreaIdentifier, doy) %>%
        summarize(totYears = n()) %>%
        as.data.frame()
      
  mig.data <- left_join(df.totYears, df.nyears, by = c("SurveyAreaIdentifier", "doy")) 
      
  mig.data<-mig.data %>% mutate(prop_years = nyears/totYears, n_years = totYears) %>%
        select(-nyears, -totYears) %>% left_join(df.abund, by = c("SurveyAreaIdentifier", "doy"))
  
  mig.data$species_id<-species_id
  mig.data$species<-species
  
  mig.data<-mig.data %>% relocate(species_id, .after=SurveyAreaIdentifier) %>% relocate (species, .after=species_id)

  #write the species specific output to the site specific table  
  write.table(mig.data, 
              file = paste(out.dir, site, "_SuperData.csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
   } #end if nrow
} #end sp loop
} #end try error


