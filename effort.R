source("00_setup.R")

  collection <- as.character(anal.param[t, "collection"])
  station <- as.character(anal.param[t, "station"])
  site <- as.character(anal.param[t, "site"])
  site.specific <- anal.param[t, "site.specific"]
  min.species <- anal.param[t, "min.species"]
  use.trfl <- anal.param[t, "use.trfl"]
  responseM<-anal.param[t , "obs.var.M"]
  responseO<-anal.param[t , "obs.var.O"]
  effort <- as.character(anal.param[t, "effort"])
  

## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. First, it will look to see if you have a copy saved in the data directory. 

in.data <-read.csv(paste(data.dir, site, "_Raw_Data.csv", sep=""))

in.data <- in.data %>% select(SurveyAreaIdentifier, project_id, SiteCode, YearCollected, MonthCollected, DayCollected, EffortUnits1, EffortMeasurement1, EffortUnits2, EffortMeasurement2, EffortUnits3, EffortMeasurement3, EffortUnits4, EffortMeasurement4, EffortUnits5, EffortMeasurement5, EffortUnits6, EffortMeasurement6, EffortUnits7, EffortMeasurement7, EffortUnits8, EffortMeasurement8, EffortUnits9, EffortMeasurement9, EffortUnits10, EffortMeasurement10, EffortUnits11, EffortMeasurement11)

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

## Assign date and season variables
in.data <- in.data %>%
  mutate(date = ymd(paste(YearCollected, MonthCollected, DayCollected, sep = "/")),
         doy = yday(date),
         season = if_else(doy < 180, "Spring", "Fall"))

# Keep only the unique rows of data since the effort is the same for each day

in.data <- in.data %>% distinct(YearCollected, MonthCollected, DayCollected, .keep_all = TRUE) 

# Use the coverage doy filter from the superfile assessment to trim the data to only include the core doy
# note that the doy coverage file will need to have been generated before the effort assessment.
# this output was not saved

#doy<-read.csv(paste(data.dir, site, "_CoverageDOY.csv", sep=""))
#min.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(min))
#max.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(max))
#min.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(min))
#max.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(max))

#trim doy by seasonal windows
fall<-in.data %>% filter(season=="Fall")
windowsf <- with(fall, round(quantile(doy, probs = c(2.5, 97.5)/100, na.rm = T), digits = 0))
fall <- subset(fall, doy >= windowsf[1] & doy <= windowsf[2])

spring<-in.data %>% filter(season=="Spring")
windowss <- with(spring, round(quantile(doy, probs = c(2.5, 97.5)/100, na.rm = T), digits = 0))
spring <- subset(spring, doy >= windowss[1] & doy <= windowss[2])

#fall<- try(in.data %>% filter(season=="Fall", doy>=min.fall, doy<=max.fall), silent=TRUE)
#spring<-try(in.data %>% filter(season=="Spring", doy>=min.spring, doy<=max.spring), silent = TRUE)

in.data<-rbind(spring, fall)

#sort data and choose filter away effort rows if they are all NA
in.data<- in.data %>%
  arrange(YearCollected,doy) %>% filter(!if_all(7:28, is.na))

write.csv(in.data, paste(data.dir, site, "_Raw_Effort.csv", sep=""), row.names = FALSE)

#sort and select the desired data columns
in.data$season_f<-factor(in.data$season, levels=c("Spring", "Fall"))
in.data <- in.data %>% select(SiteCode, YearCollected, season_f, doy, EffortMeasurement1, EffortMeasurement2, EffortMeasurement4, EffortMeasurement5, EffortMeasurement6, EffortMeasurement11)

#prepare data
in.data$EffortMeasurement1<-as.numeric(in.data$EffortMeasurement1)
in.data$EffortMeasurement2<-as.numeric(in.data$EffortMeasurement2)
#in.data$EffortMeasurement3<-as.numeric(in.data$EffortMeasurement3)
in.data$EffortMeasurement4<-as.numeric(in.data$EffortMeasurement4)
in.data$EffortMeasurement5<-as.numeric(in.data$EffortMeasurement5)
in.data$EffortMeasurement6<-as.numeric(in.data$EffortMeasurement6)
#in.data$EffortMeasurement7<-as.numeric(in.data$EffortMeasurement7)
#in.data$EffortMeasurement8<-as.numeric(in.data$EffortMeasurement8)
#in.data$EffortMeasurement9<-as.numeric(in.data$EffortMeasurement9)
#in.data$EffortMeasurement10<-as.numeric(in.data$EffortMeasurement10)
in.data$EffortMeasurement11<-as.numeric(in.data$EffortMeasurement11)

#Combine ground trap hour column
in.data<-in.data %>% mutate(EffortMeasurement5 = rowSums(across(c(EffortMeasurement5, EffortMeasurement6)), na.rm=TRUE)) %>% select(-EffortMeasurement6)

#Correct census that were not entered on the decimal hour
if (site=="BBO"){
in.data<-in.data %>% mutate(EffortMeasurement4 = ifelse(EffortMeasurement4>=1, EffortMeasurement4/100, EffortMeasurement4))
}

#remove outliers
if(site=="TCBO"){
    in.data<-in.data %>% filter(EffortMeasurement1<=100)
  in.data<-in.data %>% filter(EffortMeasurement11<=20)
}  


#CREATE PLOT #1

#remove columns with all NA values
#plot.data<-plot.data[, colSums(is.na(plot.data))<nrow(plot.data)]
#melt data frame
plot.data<-in.data %>% select(-doy)
plot.data$YearCollected<-as.factor(plot.data$YearCollected)
plot.data<-plot.data %>% melt(id=c("SiteCode", "YearCollected", "season_f"))
levels(plot.data$variable)<-c("Std. Net Hours", "Std. Observer Hours", "Census/ Std. Watch", "Ground Trap Hours", "Number Observers")
plot.data<-plot.data %>% filter(!is.na(value))

out.plot<-ggplot(data = plot.data, aes(x=YearCollected, y=value, colour=season_f))+
  geom_boxplot()+
  #geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~variable, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Year")+
  ylab("Effort Units")+
  ggtitle(site)

pdf(paste(plot.dir, site, "_", "EffortPlot.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()

write.csv(plot.data, paste(out.dir, station, "_Effort.csv", sep=""), row.names = FALSE)


#CREATE PLOT #3

#A little more data wrangling to get coverage codes

#need to calculate the mode of the net-hours
#aasuming that the mode is the net hours that should be done each day

plot.data<-in.data

#Mode <- function(x) {
#  ux <- unique(x)
#  ux[which.max(tabulate(match(x, ux)))]
#}

#Use the mode to determine the what is the normal number of net hours 
#this did not work if the mode was zero
#modeM1<-Mode(in.data$EffortMeasurement1)

quant95<-as.numeric(quantile(plot.data$EffortMeasurement1, probs=0.95, na.rm=TRUE))

# if effort is part (p) this means that census data are missing and we treat the effort assessment as banding only
if(effort=="p"){
 responseO <- "ObservationCount4"
}


if(site=="TCBO"){

  #complete watch over 4.5 hours
  plot.data<-in.data %>% mutate(prop_net=EffortMeasurement1/quant95, census=ifelse(EffortMeasurement4>=4.5, "yes", "no"))
  
  plot.data<-plot.data %>% mutate(covercode=ifelse(prop_net>=0.95 & census=="yes", 5,
                                                   ifelse(prop_net<=0.95 & prop_net>=0.80 &  census =="yes", 4,
                                                          ifelse(prop_net>=0.6 & census=="no", 3, 
                                                                 ifelse(prop_net <= 0.8 & prop_net >= 0.50  & census =="yes", 3, 
                                                                        ifelse (prop_net <= 0.6 & prop_net >= 0.40 & census =="no", 2,        
                                                                                ifelse (prop_net <= 0.5 & prop_net >= 0.33 &  census =="yes", 2,  
                                                                                        ifelse (prop_net <= 0.4 & prop_net >= 0.1 & census =="no", 1, 
                                                                                                ifelse (prop_net <= 0.33 & prop_net >= 0.1 &  census =="yes", 1, 
                                                                                                        ifelse (prop_net == 0 & census == "yes", 1,             
                                                                                                                ifelse(prop_net <= 0.1 & census =="no", 0, 0)
                                                                                                      ))))))))))
  
  
}else{

if(responseO == "ObservationCount4"){ 
  
  #if banding only

plot.data<-in.data %>% mutate(prop_net=EffortMeasurement1/quant95)

#create coverage code
plot.data<-plot.data %>% mutate(covercode=ifelse(prop_net>=0.95 , 5,
                                                ifelse(prop_net<=0.95 & prop_net>=0.80, 4,
                                                    ifelse(prop_net <= 0.8 & prop_net >= 0.50, 3, 
                                                            ifelse (prop_net <= 0.5 & prop_net >= 0.33, 2, 
                                                                ifelse (prop_net <= 0.3 & prop_net >= 0.1, 1,
                                                                      ifelse(prop_net<=0.1, 0, 0)
                                                                       ))))))
}else{
  
  #if banding+census
  
  plot.data<-in.data %>% mutate(prop_net=EffortMeasurement1/quant95, census=ifelse(EffortMeasurement4>=0, "yes", "no"))
  
  plot.data<-plot.data %>% mutate(covercode=ifelse(prop_net>=0.95 & census=="yes", 5,
                                                          ifelse(prop_net<=0.95 & prop_net>=0.80 &  census =="yes", 4,
                                                                ifelse(prop_net>=0.6 & census=="no", 3, 
                                                                ifelse(prop_net <= 0.8 & prop_net >= 0.50  & census =="yes", 3, 
                                                                        ifelse (prop_net <= 0.6 & prop_net >= 0.40 & census =="no", 2,        
                                                                        ifelse (prop_net <= 0.5 & prop_net >= 0.33 &  census =="yes", 2,  
                                                                                               ifelse (prop_net <= 0.4 & prop_net >= 0.1 & census =="no", 1, 
                                                                                               ifelse (prop_net <= 0.33 & prop_net >= 0.1 &  census =="yes", 1, 
                                                                                               ifelse (prop_net == 0 & census == "yes", 1,             
                                                                                                            ifelse(prop_net <= 0.1 & census =="no", 0, 0)
                                                                                                            ))))))))))
  
  
}

  } #end if site = TCBO

                                                              
plot.data<-plot.data %>% group_by(SiteCode, YearCollected, season_f) %>% mutate(n_day=n()) %>% 
  group_by(SiteCode, season_f, YearCollected, covercode, n_day, ) %>% summarize(n_code = n()) %>% mutate(prop_day=n_code/n_day*100) %>% na.omit() 

plot.data$YearCollected<-as.factor(plot.data$YearCollected)

out.plot<-ggplot(data = plot.data, aes(x=covercode, y=prop_day, group=YearCollected))+
  geom_point(aes(color=YearCollected))+
  #geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~season_f, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Coverage Code")+
  ylab("Proportion of DOY")+
  ggtitle(site)

pdf(paste(plot.dir, site, "_", "CoverageCodePlot.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()

write.csv(plot.data, paste(out.dir, station, "_CoverageCode.csv", sep=""), row.names = FALSE)






