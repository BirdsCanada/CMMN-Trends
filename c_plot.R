
if(station == "LPBO") {
  for(m in 1:length(site.list)) {
    
    year.data<-read.csv(paste(data.dir, site, "_YearsSurvey.csv", sep=""))
    year.data$season_f<-factor(year.data$season, levels=c("Spring", "Fall"))
    year.data<- year.data[order(year.data$doy),]
    doy<-year.data %>% filter(SurveyAreaIdentifier == site.list[m])
    
    doy<-doy %>% group_by(season_f) %>% filter(prop_year>0.68) %>% summarise(min=min(doy), max=max(doy))
    write.csv(doy, paste(data.dir, site.list[m], "_CoverageDOY.csv", sep=""), row.names=FALSE)

    out.plot<-NULL
    
    ## Generate new species list for analysis
    mig.data<-read.csv(paste(data.dir, site, "_SuperData.csv", sep = ""))
    mig.data<-mig.data%>% filter(SurveyAreaIdentifier == site.list[m])
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
      
      data_startline<-doy %>% select(season_f, min)
      data_endline<- doy %>% select(season_f, max)
      
      out.plot[[n]]<-ggplot(data = mean.plot, aes(x=doy, y=value, colour=variable))+
        geom_point()+
        facet_wrap(~season_f, scales = "free", ncol=1 )+
        geom_smooth(se=FALSE)+
        geom_vline(data = data_startline, aes(xintercept = min))+
        geom_vline(data = data_endline, aes(xintercept = max))+
        # theme_classic()+
        xlab("Day of Year")+
        ylab("Mean Count")+
        ggtitle(species.list2[n])+
        scale_x_continuous(breaks=scales::breaks_width(width=10))
      
         }#end species loop
    
    
    pdf(paste(plot.dir, site.list[m], "_", "Migration.pdf", sep = ""),
        height = 10, width = 8, paper = "letter")
    
    #    print(out.plot0)
    for(r in 1:length(out.plot)){
      try(print(out.plot[[r]]), silent = TRUE)
    }
    
    while(!is.null(dev.list())) dev.off()
    
    
      } #end site.list loop      
  } #end station LPBO

mig.data<-read.csv(paste(data.dir, site, "_SuperData.csv", sep = ""))
year.data<-read.csv(paste(data.dir, site, "_YearsSurvey.csv", sep=""))

year.data$season_f<-factor(year.data$season, levels=c("Spring", "Fall"))
year.data<- year.data[order(year.data$doy),]

doy<-year.data %>% group_by(season_f) %>% filter(prop_year>0.68) %>% summarise(min=min(doy), max=max(doy))
write.csv(doy, paste(data.dir, site, "_CoverageDOY.csv", sep=""), row.names=FALSE)

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

 data_startline<-doy %>% select(season_f, min)
 data_endline<- doy %>% select(season_f, max)

out.plot[[n]]<-ggplot(data = mean.plot, aes(x=doy, y=value, colour=variable))+
  geom_point()+
  facet_wrap(~season_f, scales = "free", ncol=1 )+
  geom_smooth(se=FALSE)+
  geom_vline(data = data_startline, aes(xintercept = min))+
  geom_vline(data = data_endline, aes(xintercept = max))+
 # theme_classic()+
  xlab("Day of Year")+
  ylab("Mean Count")+
  ggtitle(species.list2[n])+
  scale_x_continuous(breaks=scales::breaks_width(width=10))
  

} #end species loop


pdf(paste(plot.dir, site, "_", "Migration.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")
  
#    print(out.plot0)
for(r in 1:length(out.plot)){
    try(print(out.plot[[r]]), silent = TRUE)
}

while(!is.null(dev.list())) dev.off()



