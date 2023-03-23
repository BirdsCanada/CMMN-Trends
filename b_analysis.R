#Create output table for site specific results
mig.data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 11, byrow = FALSE,
                                    dimnames = NULL))
names(mig.data) <- c("SurveyAreaIdentifier", "species_id", "species", "doy", "prop_years", "n_years", "season", "mean_Obs", "mean_Obs3", "mean_Obs4", "mean_Obs7")  
write.table(mig.data, file = paste(out.dir, 
                                      site, ".SuperData.csv", sep = ""), 
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
              file = paste(out.dir, site, ".SuperData.csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
   } #end if nrow
} #end sp loop
