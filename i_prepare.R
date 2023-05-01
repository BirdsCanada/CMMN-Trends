source("00_setup.R")

  collection <- as.character(anal.param[t, "collection"])
  station <- as.character(anal.param[t, "station"])
  site <- as.character(anal.param[t, "site"])
  site.specific <- anal.param[t, "site.specific"]
  min.species <- anal.param[t, "min.species"]
  use.trfl <- anal.param[t, "use.trfl"]
    
## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. First, it will look to see if you have a copy saved in the data directory. 

in.data <-try(read.csv(paste(out.dir, site, "_Raw_Effort.csv", sep="")))

if(class(in.data) == 'try-error'){

in.data <- nc_data_dl(collections = collection, fields_set = "extended", username = u, info="CMMN Effort Scripts", years=c(min.year, max.year))

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

#sort data and choose filter away effort columns if they are all NA
in.data<- in.data %>%
  arrange(YearCollected,doy) %>% filter(!if_all(7:28, is.na))

#prepare data
in.data$season_f<-factor(in.data$season, levels=c("Spring", "Fall"))
in.data$EffortMeasurement1<-as.numeric(in.data$EffortMeasurement1)
in.data$EffortMeasurement2<-as.numeric(in.data$EffortMeasurement2)
in.data$EffortMeasurement3<-as.numeric(in.data$EffortMeasurement3)
in.data$EffortMeasurement4<-as.numeric(in.data$EffortMeasurement4)
in.data$EffortMeasurement5<-as.numeric(in.data$EffortMeasurement5)
in.data$EffortMeasurement6<-as.numeric(in.data$EffortMeasurement6)
in.data$EffortMeasurement7<-as.numeric(in.data$EffortMeasurement7)
in.data$EffortMeasurement8<-as.numeric(in.data$EffortMeasurement8)
in.data$EffortMeasurement9<-as.numeric(in.data$EffortMeasurement9)
in.data$EffortMeasurement10<-as.numeric(in.data$EffortMeasurement10)
in.data$EffortMeasurement11<-as.numeric(in.data$EffortMeasurement11)


#create plot dataframe
plot.data<-in.data %>% select(-SurveyAreaIdentifier, -project_id, -date, -EffortUnits1, -EffortUnits2, -EffortUnits3, -EffortUnits4, -EffortUnits5, -EffortUnits6, -EffortUnits7, -EffortUnits8, -EffortUnits9, -EffortUnits10, -EffortUnits11) 
#create year specific summary
#plot.data<-plot.data %>% group_by(SiteCode, YearCollected, season) %>% summarize(mean1=mean(EffortMeasurement1, na.rm=TRUE), sd1=sd(EffortMeasurement1, na.rm=TRUE), mean2=mean(EffortMeasurement2, na.rm=TRUE), sd2=sd(EffortMeasurement2, na.rm=TRUE), mean3=mean(EffortMeasurement3, na.rm=TRUE), sd3=sd(EffortMeasurement3, na.rm=TRUE), mean4=mean(EffortMeasurement4, na.rm=TRUE), sd4=sd(EffortMeasurement4, na.rm=TRUE), mean5=mean(EffortMeasurement5, na.rm=TRUE), sd5=sd(EffortMeasurement5, na.rm=TRUE), mean6=mean(EffortMeasurement6, na.rm=TRUE), sd6=sd(EffortMeasurement6, na.rm=TRUE), mean7=mean(EffortMeasurement7, na.rm=TRUE), sd7=sd(EffortMeasurement7, na.rm=TRUE), mean8=mean(EffortMeasurement8, na.rm=TRUE), sd8=sd(EffortMeasurement8, na.rm=TRUE), mean9=mean(EffortMeasurement9, na.rm=TRUE), sd9=sd(EffortMeasurement9, na.rm=TRUE), mean10=mean(EffortMeasurement10, na.rm=TRUE), sd10=sd(EffortMeasurement10, na.rm=TRUE), mean11=mean(EffortMeasurement11, na.rm=TRUE), sd11=sd(EffortMeasurement11, na.rm=TRUE))
#remove columns with all NA values
plot.data<-plot.data[, colSums(is.na(plot.data))<nrow(plot.data)]
#melt data frame
#plot.data<-plot.data %>% melt(id=c("SiteCode", "YearCollected", "season"))
plot.data<-plot.data %>% melt(id=c("SiteCode", "YearCollected", "MonthCollected", "DayCollected", "doy", "season"))
plot.data$season_f<-factor(plot.data$season, levels=c("Spring", "Fall"))
plot.data$YearCollected<-as.numeric(plot.data$YearCollected)
plot.data<-plot.data %>% filter(!is.na(value))

out.plot<-ggplot(data = plot.data, aes(x=YearCollected, y=value, colour=variable))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~season_f, scales = "free",  ncol=1)+
  theme_classic()+
  xlab("Year")+
  ylab("Effort")+
  ggtitle(site)

pdf(paste(out.dir, site, "_", "EffortPlot.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")
 
 try(print(out.plot), silent = TRUE)

while(!is.null(dev.list())) dev.off()


} #end loop 

