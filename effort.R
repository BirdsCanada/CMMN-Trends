source("00_setup.R")

  collection <- as.character(anal.param[t, "collection"])
  station <- as.character(anal.param[t, "station"])
  site <- as.character(anal.param[t, "site"])
  site.specific <- anal.param[t, "site.specific"]
  min.species <- anal.param[t, "min.species"]
  use.trfl <- anal.param[t, "use.trfl"]
    
## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. First, it will look to see if you have a copy saved in the data directory. 

in.data <-try(read.csv(paste(data.dir, site, "_Raw_Data.csv", sep=" ")))

if(class(in.data) == 'try-error'){

  paste("Must have full data downloaded and saved to run this process. Please run: 03-LoadManipSite")
  
}  #end try error
  
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

## Assign date and season variables
in.data <- in.data %>%
  mutate(date = ymd(paste(YearCollected, MonthCollected, DayCollected, sep = "/")),
         doy = yday(date),
         season = if_else(doy < 180, "Spring", "Fall"))

# Keep only the unique rows of data since the effort is the same for each day

in.data <- in.data %>% distinct(YearCollected, MonthCollected, DayCollected, .keep_all = TRUE) 

# Use the coverage doy filter from the superfile assessment to trim the data to only include the core doy
# note that the doy coverage file will need to have been generated before the effort assessment. 

doy<-read.csv(paste(out.dir, site, "_CoverageDOY.csv"))
min.spring<-doy[1,2]
max.spring<-doy[1,3]
min.fall<-doy[2,2]
max.fall<-doy[2,3]
  
fall<-in.data %>% filter(season=="Fall", doy>=min.fall, doy<=max.fall)
spring<-in.data %>% filter(season=="Spring", doy>=min.spring, doy<=max.spring)
in.data<-rbind(fall, spring)

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


#CREATE PLOT #1

#remove columns with all NA values
#plot.data<-plot.data[, colSums(is.na(plot.data))<nrow(plot.data)]
#melt data frame
plot.data<-in.data %>% melt(id=c("SiteCode", "YearCollected", "doy", "season_f"))
plot.data$YearCollected<-as.numeric(plot.data$YearCollected)
levels(plot.data$variable)<-c("Std. Net Hours", "Std. Observer Hours", "Census/ Std. Watch", "Number Observers")
plot.data<-plot.data %>% filter(!is.na(value))

out.plot<-ggplot(data = plot.data, aes(x=YearCollected, y=value, colour=variable))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~season_f, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Year")+
  ylab("Effort")+
  ggtitle(site)

pdf(paste(plot.dir, site, "_", "EffortPlot1.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")
 
 try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()

#CREATE PLOT #2

#remove columns with all NA values
#plot.data<-plot.data[, colSums(is.na(plot.data))<nrow(plot.data)]
#melt data frame
plot.data<-in.data %>% select(-doy)
plot.data$YearCollected<-as.factor(plot.data$YearCollected)
plot.data<-plot.data %>% melt(id=c("SiteCode", "YearCollected", "season_f"))
levels(plot.data$variable)<-c("Std. Net Hours", "Tot. Observer Hours", "Census/ Std. Watch", "Number Observers")
lot.data<-plot.data %>% filter(!is.na(value))

out.plot<-ggplot(data = plot.data, aes(x=YearCollected, y=value, colour=variable))+
  geom_boxplot()+
  #geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~season_f, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Year")+
  ylab("Effort")+
  ggtitle(site)

pdf(paste(plot.dir, site, "_", "EffortPlot2.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()


#CREATE PLOT #3

#A little more data wrangling to get coverage codes

#need to calculate the mode of the net-hours
#aasuming that the mode is the net hours that should be done each day

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Use the mode to determine the what is the normal number of net hours 
modeM1<-Mode(in.data$EffortMeasurement1)

plot.data<-in.data %>% mutate(prop_net=EffortMeasurement1/modeM1, census=ifelse(EffortMeasurement4>=0, "yes", "no"))

#create coverage code
plot.data<-plot.data %>% mutate(covercode=ifelse(prop_net>=0.95 & census=="yes", 5,
                                          ifelse(prop_net>=0.95 & census=="no", 2, #this is not in the table
                                                 ifelse(prop_net<=0.95 & prop_net>=0.80 &  census =="yes", 4,
                                                ifelse(prop_net<=0.95 & prop_net>=0.80 &  census =="no", 2, #this is not in table        
                                                        ifelse(prop_net <= 0.8 & prop_net >= 0.50  & census =="yes", 3, 
                                                        ifelse (prop_net <= 0.8 & prop_net >= 0.50 & census =="no", 2, #changed from 40-60 for consistency        
                                                              ifelse (prop_net <= 0.5 & prop_net >= 0.3 &  census =="yes", 2, #changed from 33% to 30% 
                                                              ifelse (prop_net <= 0.5 & prop_net >= 0.3 & census =="no", 1, #change from 20% to 30%
                                                                      ifelse (prop_net <= 0.3 & prop_net >= 0.1 &  census =="yes", 1, #changed from 33% to 30%
                                                                      ifelse (prop_net <= 0.3 & prop_net >= 0.1 & census =="no", 0,
                                                                              ifelse(prop_net<=0.1 & census=="yes", 1, 0)
                                                                                  )))))))))))



                                                              
plot.data<-plot.data %>% group_by(SiteCode, YearCollected, season_f) %>% mutate(n_day=n()) %>% 
  group_by(SiteCode, YearCollected, season_f, covercode, n_day, ) %>% summarize(n_code = n()) %>% mutate(prop_day=n_code/n_day*100) %>% na.omit() 

out.plot<-ggplot(data = plot.data, aes(x=covercode, y=prop_day))+
  geom_point(aes(size = prop_day))+
  #geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~season_f, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Year")+
  ylab("Coverage Code")+
  ggtitle(site)

pdf(paste(plot.dir, site, "_", "EffortPlot3.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()






