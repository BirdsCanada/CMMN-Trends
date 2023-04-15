source("00_setup.R")

mig.data<-read.csv(paste(out.dir, site, "_SuperData.csv", sep = ""))


year.data<-read.csv(paste(out.dir, site, "_YearsSurvey.csv", sep=""))
year.data$season_f<-factor(year.data$season, levels=c("Spring", "Fall"))
year.data<- year.data[order(year.data$doy),]

doy<-year.data %>% group_by(season_f) %>% filter(prop_year>0.68) %>% summarise(min=min(doy), max=max(doy))


out.plot<-NULL

## Generate new species list for analysis
mig.data<-mig.data %>% filter(!grepl("UN", species))
species.list2 <- unique(mig.data$species)
species.list2<-na.omit(species.list2)
species.list2<-sort(species.list2)

#species loop
for(n in 1:length(species.list2)) {
  
  plot.data<-NULL
  plot.data <- filter(mig.data, species == species.list2[n]) 
  sp.name<-unique(plot.data$species)

  mean.plot<-plot.data %>% melt(id.vars=c("SurveyAreaIdentifier", "species", "doy", "season"))   
  mean.plot$variable <- factor(mean.plot$variable , levels=c("mean_Obs", "mean_Obs3", "mean_Obs4", "mean_Obs7"), labels=c("DET", "Census", "Banding", "B+C"))

  #omit NA
 mean.plot<-na.omit(mean.plot)
 mean.plot$season_f<-factor(mean.plot$season, levels=c("Spring", "Fall"))
 
 #filter out DOY <90
 
 mean.plot<- mean.plot %>% filter(doy>90)

 #create the vlines for different groups
 #fall_start_95<-list(doy[1,2])
 #fall_end_95<-list(doy[1,3])
 #spring_start_95<-list(doy[2,2])
 #spring_end_95<-list(doy[2,3])
 #data_startline<-data.frame(group=unique(year.data$season_f), vline=c(spring_start_95, fall_start_95))
 #data_endline<-data.frame(group=unique(year.data$season_f), vline=c(spring_end_95, fall_end_95))
  
 data_startline<-doy %>% select(season_f, min)
 data_endline<- doy %>% select(season_f, max)

out.plot[[n]]<-ggplot(data = mean.plot, aes(x=doy, y=value, colour=variable))+
  geom_point()+
  facet_wrap(~season_f, scales = "free_x", ncol=1 )+
  geom_smooth(se=FALSE)+
  geom_vline(data = data_startline, aes(xintercept = min))+
  geom_vline(data = data_endline, aes(xintercept = max))+
 # theme_classic()+
  xlab("Day of Year")+
  ylab("Mean Count")+
  ggtitle(species.list2[n])+
  scale_x_continuous(breaks=scales::breaks_width(width=10))
  

} #end species loop

#Print proportion of years a doy was surveyed
out.plot0<-ggplot()+
  geom_point(year.data, aes(x=doy, y=prop_year, colour=season))+
  geom_point(doy, aes(x=doy, y=prop_year))+
  facet_wrap(~season_f, scales = "free", ncol=1)+
  geom_smooth(se=FALSE)+
  geom_hline(yintercept = 0.67)+
  xlab("Day of Year")+
  ylab("Proportion doy surveyed")+
  #theme_classic()+
  scale_x_continuous(breaks=scales::breaks_width(width=10))
  

pdf(paste(out.dir, site, "_", "Migration.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")
  
    print(out.plot0)
for(r in 1:length(out.plot)){
    try(print(out.plot[[r]]), silent = TRUE)
}

while(!is.null(dev.list())) dev.off()



