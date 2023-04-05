source("00_setup.R")

mig.data<-read.csv(paste(out.dir, site, "_SuperData.csv", sep = ""))
year.data<-read.csv(paste(out.dir, site, "_YearsSurvey.csv", sep=""))
year.data$season_f<-factor(year.data$season, levels=c("Spring", "Fall"))

out.plot<-NULL

## Generate new species list for analysis
mig.data<-mig.data %>% filter(!grepl("UN", species))
species.list2 <- unique(mig.data$species)
species.list2<-na.omit(species.list2)

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

 #create the vlines for different groups
 #fall_start_95<-max(mig.data$start_95, na.rm = TRUE)
 #fall_end_95<-max(mig.data$end_95, na.rm = TRUE)
  #spring_start_95<-min(mig.data$start_95, na.rm = TRUE)
 #spring_end_95<-min(mig.data$end_95, na.rm = TRUE)
 #data_startline<-data.frame(group=unique(mean.plot$season_f), vline=c(spring_start_95, fall_start_95))
 #data_endline<-data.frame(group=unique(mean.plot$season_f), vline=c(spring_end_95, fall_end_95))
    

out.plot[[n]]<-ggplot(data = mean.plot, aes(x=doy, y=value, colour=variable))+
  geom_point()+
  facet_wrap(~season_f, scales = "free_x" )+
  geom_smooth()+
 # geom_vline(data = data_startline, aes(xintercept = vline))+
 # geom_vline(data = data_endline, aes(xintercept = vline))+
 # theme_classic()+
  xlab("Day of Year")+
  ylab("Mean Count")+
  ggtitle(species.list2[n])
  

} #end species loop

#Print proportion of years a doy was surveyed
out.plot0<-ggplot(year.data, aes(x=doy, y=prop_year, colour=season))+
  geom_point()+
  facet_wrap(~season_f, scales = "free" )+
  geom_smooth()+
  # theme_classic()+
  xlab("Day of Year")+
  ylab("Proportion doy surveyed")

pdf(paste(out.dir, site, "_", "Migration.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")
  
    print(out.plot0)
for(r in 1:length(out.plot)){
    try(print(out.plot[[r]]), silent = TRUE)
}

while(!is.null(dev.list())) dev.off()



