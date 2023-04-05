
# Species Specific loop
  
for(k in 1:length(species.list)) {
  sp.data<-NULL
  sp.data <- filter(in.data, SpeciesCode == species.list[k]) %>%
    select(-start_date, -end_date, -lpbo_combine, -analysis_code,) %>%
    droplevels()

  sp.id <- unique(sp.data$species_id)
  species <- species.list[k]

  ## ZERO-FILL DATA by merging event and real data. This is based on the DET column.  
  
  sp.data <- left_join(event.data, sp.data, by = c("SurveyAreaIdentifier",  "YearCollected", "MonthCollected", "DayCollected", "date", "doy", "season")) %>%
    mutate(
      ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0),
      species_id = sp.id,
      SpeciesCode = species)
  
  ## FILTER DATA FOR SPECIES-SPECIFIC "BAD DATES"
  
  site.list <- as.character(unique(sp.data$SurveyAreaIdentifier))
  
  sp.data <- bscdata.filterBadDates(sp.data, sitecode = site.list, species = sp.id)
  
  ## FILTER SPECIES-SPECIFIC MIGRATION WINDOWS. 
  ## Filter by seasonal migration windows BEFORE doing additional filters (min number years, mean abundance, number of detection days), as those should be applied to the data within a window
  
  ## Migration windows are included in the superfile.
  
  if(site != "LPBO") {
    df.migWindows <- df.superfile %>%
      filter(SpeciesCode == species.list[k]) %>%
      mutate(SpeciesCode = as.character(SpeciesCode),
             season = as.character(season)) %>%
      select(SurveyAreaIdentifier, SpeciesCode, season, analysis_code, start_date, end_date) %>%
      droplevels()
  }
  
  ### ** This looks the same as above, check against original code**
  if(site == "LPBO") {
    df.migWindows <- df.superfile %>%
      filter(SpeciesCode == species.list[k]) %>%
      mutate(SpeciesCode = as.character(SpeciesCode),
             season = as.character(season)) %>%
      select(SurveyAreaIdentifier, SpeciesCode, season, analysis_code, start_date, end_date) %>%
      droplevels()
  }
  
  sp.data <- left_join(df.migWindows, sp.data, by = c("SurveyAreaIdentifier", "SpeciesCode", "season"), multiple="all")
  
  sp.data <- sp.data %>% filter(doy >= start_date, doy <= end_date)
  
  sp.data$YearCollected<-as.character(sp.data$YearCollected)
  sp.data$MonthCollected<-as.numeric(sp.data$MonthCollected)
  sp.data$DayCollected<-as.numeric(sp.data$DayCollected)
  
  ## FILTER SPECIES THAT DON'T MEET MIN NUMBER OF YEARS
  ## This is done based on the analysis code of the species 
  ## Because analysis code can vary on season, we need to do this within the season loop
  ## M are generally Band + Census and others are DET, detailed in the analysis parameters file. 
  ## species that don't meet # years detected (by site) requirement.
  ## Filter out species that were not observed on a minimum number of years surveyed. Currently needs to be detected on at least half of all years surveyed.  
  ## This may cause problems if only detected in last half of years surveyed.
  
  ## number of years a species must be detected on to be analyzed, currently defined as half of the years surveyed.
  
  seas.list <- as.character(unique(sp.data$season))
  
  for (j in 1:length(seas.list)) {
    
    df.tmp <- droplevels(subset(sp.data, season == seas.list[j]))
    analysis_code <- unique(df.tmp$analysis_code)
    
    if(nrow(df.tmp) > 0) { 
    
  if(analysis_code=="M"){
  
    df.tmp <- df.tmp %>% dplyr::select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season, SpeciesCode, species_id, anal.param[t , "obs.var.M"], start_date, end_date, analysis_code) 
    # rename count variable, so consistent for following steps
    names(df.tmp)[10] <- c("ObservationCount") 
    
    
  df.nyears <- df.tmp %>%
    filter(ObservationCount > 0) %>%
    select(SurveyAreaIdentifier, YearCollected, season, SpeciesCode) %>%
    distinct() %>%
    group_by(SurveyAreaIdentifier, season, SpeciesCode) %>%
    dplyr::summarize(nyears = dplyr::n()) %>%
    filter(nyears >= min.yrs.detect)%>%
    as.data.frame()
  
  df.tmp <- left_join(df.nyears, df.tmp, by = c("SurveyAreaIdentifier", "season", "SpeciesCode")) %>%
    select(-nyears)
  
  ## FILTER SPECIES THAT DON'T MEET ABUNDANCE AND OCCURRENCE REQUIREMENTS
  
  ## Want to use 0-observation counts to get mean count across years, but drop 0-observation counts to get number of non-0 observation days, so do this in separate steps:
  
  ## 1. Drop seasons where mean number of individuals/year is < 10
  
  df.abund <- NULL
  df.abund <- df.tmp %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(count = sum(ObservationCount)) %>% 
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanCount = mean(count, na.rm = TRUE)) %>%
    filter(meanCount >= 10)%>%
    as.data.frame()
  
  df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season")) %>%
    select(-meanCount)%>%
    as.data.frame()
  
  ## 2. Drop seasons where mean number of observation days/year is < 5
  
  df.abund <- NULL
  df.abund <- df.tmp %>%
    filter(ObservationCount > 0) %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(nobs = n()) %>%
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanObsDays = mean(nobs, na.rm = TRUE)) %>%
    filter(meanObsDays >= 5) %>%
    as.data.frame()
  
  df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season")) %>%
    select(-meanObsDays) %>%
    as.data.frame()
  
  } #end if analysis code = M
  
#select the other response variable if not M species or if M species does not meet minimum data requirement. 
  if(analysis_code!="M"|nrow(df.tmp) == 0){

    df.tmp <- df.tmp %>% dplyr::select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season, SpeciesCode, species_id, anal.param[t , "obs.var.O"], start_date, end_date, analysis_code) 
    # rename count variable, so consistent for following steps
    names(df.tmp)[10] <- c("ObservationCount") 
    
    df.nyears <- NULL
    df.nyears <- df.tmp %>%
      filter(ObservationCount > 0) %>%
      select(SurveyAreaIdentifier, YearCollected, season, SpeciesCode) %>%
      distinct() %>%
      group_by(SurveyAreaIdentifier, season, SpeciesCode) %>%
      dplyr::summarize(nyears = dplyr::n()) %>%
      filter(nyears >= min.yrs.detect)%>%
      as.data.frame()
    
    df.tmp <- left_join(df.nyears, df.tmp, by = c("SurveyAreaIdentifier", "season", "SpeciesCode"), multiple="all") %>%
      select(-nyears)
    
    ## FILTER SPECIES THAT DON'T MEET ABUNDANCE AND OCCURRENCE REQUIREMENTS
    
    ## Want to use 0-observation counts to get mean count across years, but drop 0-observation counts to get number of non-0 observation days, so do this in separate steps:
    
    ## 1. Drop seasons where mean number of individuals/year is < 10
    
    df.abund <- NULL
    df.abund <- df.tmp%>%
      group_by(SurveyAreaIdentifier, season, YearCollected) %>%
      summarize(count = sum(ObservationCount , na.rm = TRUE)) %>% 
      group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
      summarize(meanCount = mean(count, na.rm = TRUE)) %>%
      filter(meanCount >= 10)%>%
      as.data.frame()
    
    df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
      select(-meanCount)%>%
      as.data.frame()
    
    ## 2. Drop seasons where mean number of observation days/year is < 5
    
    df.abund <- NULL
    df.abund <- df.tmp %>%
      filter(ObservationCount > 0) %>%
      group_by(SurveyAreaIdentifier, season, YearCollected) %>%
      summarize(nobs = n()) %>%
      group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
      summarize(meanObsDays = mean(nobs, na.rm = TRUE)) %>%
      filter(meanObsDays >= 5) %>%
      as.data.frame()
    
    df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
      select(-meanObsDays) %>%
      as.data.frame()
    
  } #end if analysis code != M
    } #end if nrow df.tmp
  
#continue if there is data
  if(nrow(df.tmp) > 0) {
    
# CALCULATE ANNUAL INDICES AND TRENDS
    
    # turn species code into factor
    df.tmp <- df.tmp %>%
      mutate(SpeciesCode = as.factor(SpeciesCode)) %>%
      droplevels()
    
    # get list of sites
    
    site.list <- as.character(unique(df.tmp$SurveyAreaIdentifier))
    
#   ####################################################################
    ####################################################################
   
    #standardize variables
    #standardize year to current year  
    
    df.tmp$YearCollected<-as.numeric(df.tmp$YearCollected)
    df.tmp <- df.tmp %>% mutate(cyear = YearCollected - max.year, 
                 yearfac = as.factor(cyear),
                 fyear = as.factor(YearCollected),
                 scale.doy=scale(doy), 
                 doyfac = as.factor(scale.doy)) %>%
            select(-scale.doy)
    
    #Centralize and standardize doy and doy^2 with legendre polynomials
        tmp <- poly.legendre(df.tmp$doy, 2, min(df.tmp$doy), max(df.tmp$doy))
        names(tmp) <- c("doy1", "doy2")
        df.tmp <- cbind(df.tmp, tmp)
        
        # get years in dataset for analysis
        
        years.list <- unique(df.tmp$YearCollected)
        
        
        # MODEL FORMULAS
        
        if(length(unique(df.tmp$SurveyAreaIdentifier)) == 1) {                                         
          trend.formula <- ObservationCount ~ cyear + doy1 + doy2 + 
            f(yearfac, model = "ar1") + f(doyfac, model = "ar1") 
          
          index.formula <- ObservationCount ~ fyear - 1 + doy1 + doy2 + 
            f(yearfac, model = "ar1") + f(doyfac, model = "ar1")
        }
        
        if(length(unique(df.tmp$SurveyAreaIdentifier)) > 1) {                                         
          trend.formula <- ObservationCount ~ cyear + doy1 + doy2 + 
            SurveyAreaIdentifier + doy1*SurveyAreaIdentifier + doy2*SurveyAreaIdentifier +
            f(yearfac, model = "ar1") + f(doyfac, model = "ar1")
          
          index.formula <- ObservationCount ~ fyear - 1 + doy1 + doy2 + SurveyAreaIdentifier +
            doy1*SurveyAreaIdentifier + doy2*SurveyAreaIdentifier +
            f(yearfac, model = "ar1") + f(doyfac, model = "ar1")
        }
        
        # AUTO-GENERATE TIME PERIODS TO ANALYZE TRENDS
        
        time.period = NA
        Y1.trend = NA
        Y2.trend = NA
        
        # if the time periods are not provided, generate all-years and 10 year periods.
        
        if (is.na(time.period)) {
          endyr <- max(df.tmp$YearCollected)
          startyr <- min(df.tmp$YearCollected)
          
          Y1.trend <- startyr
          Y2.trend <- endyr
          time.period <- "all years"
          
          g <- 1
          while (endyr - (g * yr.incr) > startyr) {
            Y1.trend <- c(Y1.trend, endyr - (g * yr.incr))
            Y2.trend <- c(Y2.trend, endyr)
            time.period <- c(time.period, paste((g * yr.incr), "-years", sep=""))
            g <- g + 1
          } # end of while loop
          
          if((site == "LPBO" & startyr <= 1968)) {
            
            time.period = c(time.period, "BBS")
            Y1.trend <- c(Y1.trend, 1968)
            Y2.trend <- c(Y2.trend, endyr)
          }
          
        } # end of time period loop
        
        # TRENDS: SPECIFIC TO SITE, SEASON, SPECIES, and TIME PERIOD
        # LOOP THROUGH TIME PERIODS
        
        for(p in 1:length(time.period)) {
          
          period <- time.period[p]
          Y1 <- Y1.trend[p]
          Y2 <- Y2.trend[p]
          
          tmp.dat <- droplevels(subset(df.tmp, YearCollected %in% c(Y1:Y2)))
  
          ##RUN TREND ANALYSIS
          #Run both a nbinomial and poisson model. Select the model with the lowest AIC or WIC.                
            
            trend.nb <-trend.pois <- NULL
      
            trend.nb <- try(inla(trend.formula,family = "nbinomial", data = df.tmp,
                              control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE)), silent = T)
            
            trend.pois <- try(inla(trend.formula,family = "poisson", data = df.tmp,
                              control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE)), silent = T)
            
            top.model <- try(if(trend.nb[["dic"]][["dic"]] < trend.pois[["dic"]][["dic"]]) {trend.nb
            } else {
              trend.pois
            }
            , silent = TRUE)
            
           family <- try(if(trend.nb[["dic"]][["dic"]] < trend.pois[["dic"]][["dic"]]) {family="nbinomial"
            } else {
              family="poisson"
            }
            , silent = TRUE)
            
            #What to do if there is an error in the top.model		
            
            if(class(top.model) == 'try-error'| is.null(top.model)){
              
              error$Site<-station
              error$Season<-seas.list[j]
              error$SpeciesCode<-species
              error$time.period<-period
              
              #Print final error table to file
              
              write.table(error, file = paste(out.dir, "ErrorFile.csv", sep = ""), row.names = FALSE, append = TRUE, 
                          quote = FALSE, sep = ",", col.names = FALSE)
            }	#end try-error statement
    
          
  #what to do if there is no error in the try error above for either INLA function  	
  #first define the is.not.null function
  
          is.not.null <- function(x) !is.null(x)
          
          if(class(top.model) != 'try-error'& is.not.null(top.model)){
            
   # Calculate posterior probability that trend is negative
           
            post_year <- top.model$marginals.fixed$cyear
            
            sample <- inla.rmarginal(10000, post_year)
            
            post_prob <- ifelse(summary(top.model)$fixed["cyear", "mean"] <= 0, 
                                length(sample[sample<=0])/length(sample),  
                                length(sample[sample>0])/length(sample))
            
            # Output trend info  
            trend.out <- NULL
            trend.out <- as.data.frame(t(summary(top.model)$fixed["cyear",])) 
            names(trend.out) <- c("mean", "sd", "lcl", "mid", "ucl", "mode", "kld")
            trend.out <- trend.out %>%
              mutate(trnd = 100*(exp(trend.out$mean)-1),
                     lower_ci = 100*(exp(trend.out$lcl)-1),
                     upper_ci = 100*(exp(trend.out$ucl)-1), 
                     stdeer=sd,
                     model_type="log-linear slope", 
                     post_prob = post_prob,###Needs fixed 
                     species_code = species,
                     species_id=species_id,
                     years = paste(Y1, "-", Y2, sep = ""),###may need adjusted
                     period = period,###Needs fixed
                     season = seas.list[j],
                     results_code = results.code,
                     area_code = station,
                     site = site,
                     model_type = family,##Needs fixed. Either NB or Poisson 
                     version=max.yr.filt,
                     analysis_code = analysis_code) 
            
            if(period == "all years") {
              
              # ANNUAL INDICES: GENERATED FOR FULL TIME PERIOD ONLY
              # SPECIFIC TO SITE, SEASON, SPECIES (outside time period loop)
              # use same data distribution as for trends
              # use the index model do the trend line does not influnce the indicies generated  
              
              index <- NULL
              nyears <- length(unique(tmp.dat$YearCollected))
              
              #family is assigned based on the top trend model
              index <- try(inla(index.formula, family = family, data = tmp.dat,E = YrObs,
                                  control.predictor = list(compute = TRUE),
                                  control.compute = list(config = TRUE)), silent=T)
                
              } # end of if data model statement
              
              if(data.model == "Poisson") {
                index <- NULL
                index <- try(inla(index.formula, family = "poisson", data = tmp.dat, #E = YrObs,
                                  control.predictor = list(compute = TRUE),
                                  control.compute = list(config = TRUE)), silent=T)
                
              } # end of if data model statement
              
              
              
              # Calculate abundance indices using the posterior sample of the model
              
              if(class(index) != 'try-error'& is.not.null(index)){
                
                nsamples<- 1000  
                nyears <- length(unique(tmp.dat$fyear))
                post.sample1<-NULL #clear previous
                post.sample1<-inla.posterior.sample(nsamples, index)
                tmp1 <- select(tmp.dat, SpeciesCode, YearCollected, doy, ObservationCount)
                
                #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
                
                pred.yr.log<-matrix(nrow=nsamples, ncol=nyears)
                
                for (k in 1:nsamples){
                  tmp1$pred.log<-post.sample1[[k]]$latent[1:nrow(tmp.dat)]
                  pred.yr.log[k,]<-t(with(tmp1, aggregate (pred.log, list(YearCollected=YearCollected), mean, na.action=na.omit))$x)
                }
                
                if(!is.null(index)) {
                  mn.yr1 <- NULL
                  
                  mn.yr1<-matrix(nrow=nyears, ncol=4)
                  
                  for(g in 1:nyears){
                    mn.yr1[g,1]<-mean(pred.yr.log[,g], na.rm=TRUE)
                    mn.yr1[g,2]<-quantile(pred.yr.log[,g], 0.025, na.rm=TRUE)
                    mn.yr1[g,3]<-quantile(pred.yr.log[,g], 0.975, na.rm=TRUE)
                    mn.yr1[g,4]<-sd(pred.yr.log[,g], na.rm=TRUE)
                  }
                  
                  mn.yr1 <- as.data.frame(mn.yr1)
                  names(mn.yr1) <- c("index", "lower_ci", "upper_ic", "SD")
                  mn.yr1$SpeciesCode <- species
                  mn.yr1$index.bt <-exp(mn.yr1$index)
                  mn.yr1$lower_ci.bt <- exp(mn.yr1$lower_ci)
                  mn.yr1$upper_ci.bt <- exp(mn.yr1$upper_ic)
                  mn.yr1$SD.bt <- exp(mn.yr1$SD)
                  mn.yr1$years <- paste(min(df.tmp$YearCollected), "-", max(df.tmp$YearCollected), sep = "")
                  mn.yr1$year <- sort(unique(df.tmp$YearCollected))
                  mn.yr1$period <-"all years"
                  mn.yr1$season <- seas.list[j]
                  mn.yr1$results_code <- results.code
                  mn.yr1$area_code <- station
                  mn.yr1$analysis_code <- analysis_code
                  mn.yr1$model_type <- data.model
                  mn.yr1<- merge(mn.yr1, sp.names, by.x=c("SpeciesCode"), by.y=c("species_code"), all.x=TRUE)
                  
                  
                  ######################################################################################  
                  
                  write.table(mn.yr1, 
                              file = paste(out.dir,	site, ".AnnualIndices.", max.yr.filt, ".csv", sep = ""),
                              row.names = FALSE, 
                              append = TRUE, 
                              quote = FALSE, 
                              sep = ",", 
                              col.names = FALSE)
                  
                  #new code writes the sql output for all speices  
                  if((write.trends.sql)){
                    
                    #changes to point the sql code to the bt index table 
                    
                    #remove extreme outliers for plotting
                    
                    mn.yr1$lower_ci.bt <- ifelse(mn.yr1$SD >= 4, 0, mn.yr1$lower_ci.bt)
                    mn.yr1$upper_ci.bt <- ifelse(mn.yr1$SD >= 4, 0, 	mn.yr1$upper_ci.bt)
                    mn.yr1$SD.bt <- ifelse(mn.yr1$SD >= 4, 0, 	mn.yr1$SD.bt)
                    
                    sql.temp <- select(mn.yr1, index = index.bt, stdev = SD.bt, lower_ci = lower_ci.bt, upper_ci = upper_ci.bt, year, season, period, results_code, area_code, species_id)
                    
                    
                    bscdata.sqlSave(tablename = "results_annual_indices",
                                    df = sql.temp, 
                                    textfile = paste(out.dir, "sql_scripts/", site, "_", seas.list[j], "_results_annual_indices.sql",sep = ""),
                                    append = T, 
                                    savetable = F, 
                                    datasource = NA)
                    
                  } # end of if write sql statement
                } # end of is.null indices statement
              } # end of try.error index
              
            } # end of if time period == all years statement
            
            write.table(trend.out, 
                        file = paste(out.dir, site, ".Trends.",	max.yr.filt, ".csv", sep = ""), 
                        row.names = FALSE, 
                        append = TRUE, 
                        quote = FALSE, 
                        sep = ",", 
                        col.names = FALSE)
            
            if((write.trends.sql)) {
              
              sql.temp <- select(trend.out, 
                                 trnd, upper_ci, lower_ci, post_prob, area_code, season, model_type, results_code, period, years, species_id)
              
              bscdata.sqlSave(tablename = "results_trends",
                              df = sql.temp, 
                              textfile = paste(out.dir, "sql_scripts/", site, "_", seas.list[j], "_results_trends.sql",	sep = ""),
                              append = T, 
                              savetable = F, 
                              datasource = NA)
              
              
            } # end of if write sql statement
            
          } # end of time period loop
        } # end of if nrow statement
            } #end of for (i in 1:length(seas.list)) 
    }
    } # end of   for (j in 1:length(site.list))
    
    
  } # end of if nrow statement
  #} # end of if obscount statement
} # end of species loop



