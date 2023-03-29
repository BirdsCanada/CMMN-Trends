source("00_setup.R")

mig.data<-read.csv(paste(out.dir, site, ".SuperData.csv", sep = ""))

## Generate new species list for analysis
species.list2 <- unique(mig.data$species)
species.list2<-na.omit(species.list2)

#species loop
for(n in 1:length(species.list2)) {
  
  plot.data<-NULL
  plot.data <- filter(mig.data, species == species.list2[n]) 
  sp.name<-unique(plot.data$species)

  mean.plot<-plot.data %>% melt(id.vars=c("SurveyAreaIdentifier", "species_id", "species", "doy", "prop_years", "n_years", "season"))   
  mean.plot$variable <- factor(mean.plot$variable , levels=c("mean_Obs", "mean_Obs3", "mean_Obs4", "mean_Obs7"), labels=c("DET", "Census", "Banding", "B+C"))

pdf(paste(out.dir, site, "_", sp.name, "_", "Migration.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

#Print or plot that % against doy, and pick the doy at which % rises above 67%.  
ggplot(plot.data, aes(x=doy, y=prop_years, colour=season))+
  geom_point()+
  facet_wrap(~season, scales = "free" )+
  geom_smooth()+
  theme_classic()+
  geom_hline(yintercept=c(0.67))+
  xlab("Day of Year")+
  ylab("Proportion Years Detected")+
  ggtitle(species.list2[n])

#Print mean for each response against doy
ggplot(mean.plot, aes(x=doy, y=value, colour=variable))+
  geom_point()+
  facet_wrap(~season, scales = "free_x" )+
  geom_smooth()+
  theme_classic()+
  xlab("Day of Year")+
  ylab("Mean Count")+
  ggtitle(species.list2[n])

while(!is.null(dev.list())) dev.off()


} #end species loop
