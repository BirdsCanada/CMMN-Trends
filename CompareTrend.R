require(tidyverse)
require(ggplot2)
require(naturecounts)
require(reshape)


in.dir <- paste("Output/", max.year, "/Test/", sep = "")

site <- "ACBO"

# read in trend output

trnd2021 <- read.csv(paste(in.dir, site, "_Trends.csv", sep = ""))
trnd2021<-trnd2021 %>% filter(period=="all years") %>% select(species_code, trnd, season, index_type)
#trnd2021<-cast(trnd2021, species_code+season~index_type, value="trnd")
#trnd2021<-trnd2021 %>% dplyr::rename(trnd_endpoint_2021 = endpoint, trnd_slope_2021 = slope)

trnd2018 <- read.csv(paste(in.dir, site, ".Trends.2018.csv", sep = ""))
trnd2018<-trnd2018 %>% filter(period=="all years") %>% 
  select(SpeciesCode, trnd, season) %>% 
  dplyr::rename(species_code=SpeciesCode, 
                trnd_2018=trnd)

trnds<-NULL
trnds<-full_join(trnd2018, trnd2021, by=c("species_code", "season"), multiple="all")
#trnds<-trnds %>% select(species_code, season, trnd_2018, endpoint)#, trnd_slope_2021)

ggplot(trnds, aes(x=trnd_2018, y=trnd))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  ylim(-20, 50)+
  ylab("Trends 2021: Endpoint")+
  xlab("Trends 2018: Slope")+
  ggtitle("MBGO")+
  theme_classic()


index2021 <- read.csv(paste(in.dir, site, "_AnnualIndices.csv", sep = ""))
index2021 <- index2021 %>% filter(period=="all years") %>% select(species_code, index, season, year)

index2018<- read.csv(paste(in.dir, site, ".AnnualIndices.2018.csv", sep = ""))
index2018 <- index2018 %>% filter(period=="all years") %>% 
  dplyr::rename(species_code=SpeciesCode, index_2018=index.bt) %>% 
  select(species_code, index_2018, season, year)

options(digits = 2)
indx<-full_join(index2018, index2021, by=c("species_code", "season", "year"), multiple="all")

indx.CAGO<-indx %>% filter(species_code=="CAGO")

ggplot(indx.CAGO, aes(x=index_2018, y=index), colour=season)+
  geom_point()+
  #geom_abline(slope=1, intercept = 0)+
  #facet_wrap(~species_code)+
  ylab("Index 2021: GAM")+
  xlab("Index 2018")+
  ggtitle("MBGO")+
  theme_classic()
