
in.data<-NULL

in.data <-read.csv(paste(data.dir, site, "_Raw_Superfile.csv", sep="")) 
event.data<-read.csv(paste(data.dir, site, "_Event_Superfile.csv", sep="")) 

event.data$YearCollected<-as.integer(event.data$YearCollected)
event.data$MonthCollected<-as.integer(event.data$MonthCollected)
event.data$DayCollected<-as.integer(event.data$DayCollected)
event.data$date<-as.character(event.data$date)

#filter by station coverage window
year.data<-read.csv(paste(data.dir, site, "_YearsSurvey.csv", sep=""))
year.data$season_f<-factor(year.data$season, levels=c("Spring", "Fall"))
year.data<- year.data[order(year.data$doy),]
doy<-year.data %>% group_by(season_f) %>% filter(prop_year>0.68) %>% summarise(min=min(doy), max=max(doy))
min.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(min))
max.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(max))
min.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(min))
max.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(max))

fall<- try(in.data %>% filter(season=="Fall", doy>=min.fall, doy<=max.fall), silent=TRUE)
spring<-try(in.data %>% filter(season=="Spring", doy>=min.spring, doy<=max.spring), silent = TRUE)

open.data<-rbind(spring, fall)

#Create output table for site specific results
sp.data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 8, byrow = FALSE,
                                    dimnames = NULL))
names(sp.data) <- c("SurveyAreaIdentifier", "species", "doy", "season", "mean_Obs", "mean_Obs3", "mean_Obs4", "mean_Obs7")  
write.table(sp.data, file = paste(data.dir, 
                                      site, "_SuperData.csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#species loop
for(k in 1:length(species.list)) {
  
  sp.data<-NULL
  sp.data <- in.data %>% filter(SpeciesCode == species.list[k]) %>% distinct(SurveyAreaIdentifier,  YearCollected, MonthCollected, DayCollected, .keep_all = TRUE)
  species <- species.list[k]
  
  st.data<-NULL
  st.data <- open.data %>% filter(SpeciesCode == species.list[k]) %>% distinct(SurveyAreaIdentifier,  YearCollected, MonthCollected, DayCollected, .keep_all = TRUE)

  
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
  df.mean <- st.data %>%
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
  df.obs <- st.data %>%
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
 
#mean counts of species
      sp.data<- sp.data %>%
      group_by(SurveyAreaIdentifier, doy, season) %>%
      summarize(mean_Obs = mean(ObservationCount, na.rm = TRUE), 
                mean_Obs3 = mean(ObservationCount3, na.rm = TRUE), 
                mean_Obs4 = mean(ObservationCount4, na.rm = TRUE), 
                mean_Obs7 = mean(ObservationCount7, na.rm = TRUE)) %>% distinct() %>% as.data.frame()
       

  sp.data$SpeciesCode<-species
  
  sp.data<-sp.data %>% select(SurveyAreaIdentifier, SpeciesCode, doy, season, mean_Obs, mean_Obs3, mean_Obs4, mean_Obs7)

  #write the species specific output to the site specific table  
  write.table(sp.data, 
              file = paste(data.dir, site, "_SuperData.csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
   } #end if nrow
} #end sp loop



