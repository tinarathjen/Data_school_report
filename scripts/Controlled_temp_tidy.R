

#Load packages

library(tidyverse)
library(readxl)
library(lubridate)
library(modelr)
library(broom)



#Upload files
haundata <- read_xlsx("data/rawdata/haundata.xlsx")
tempdata <- read_csv("data/rawdata/Soilandtemp_data.csv",skip = 1)
maindata <- read_xlsx("data/Rawdata/controlled_temp_data_without_haun.xlsx")


###################Fixing column headers and column types#######################

#note MS=mainstem, t1=tiller1, fwr=flowering, spk=Spikelet number, hd=heading


maindata <- rename(maindata, 
      haun="Haun Stage", 
      emerg_date="Emergence date", 
      obs_date="Date",
      zadok="Zadock Stage",
      hd_MS="Heading MainStem", 
      hd_t1="Heading 1st Tiller",
      half_hd="50% Heading",
      fwr_MS="Flowering Mainstem",
      fwr_t1="Flowering 1st Tiller",
      half_fwr="50% Flowering",
      final_leaf="FinalLeaf#",
      spk_MS="Spikelet#_Mainstem",
      spk_t1="Spikelet#_1stTiller",
      environment="Environment",
      reps="Reps",
      genotype="Genotype") %>% 
      mutate(obs_date = ymd(obs_date), emerg_date=ymd(emerg_date))

haundata <- haundata %>% 
   rename(environment="Environment",
          reps="Reps",
          genotype="Genotype",
          haun="Haun Stage",
          emerg_date="Emergence date", 
          obs_date="Date") %>%
   mutate(obs_date = ymd(obs_date), emerg_date=ymd(emerg_date)) 
   
tempdata <- tempdata %>% 
   separate(AirTemp1,into=c("AirTemp1", "X"),sep = " " ) %>% 
   separate(AirTemp2,into=c("AirTemp2", "X2"),sep = " " ) %>% 
   separate(SoilTemp1,into=c("SoilTemp1", "X3"),sep = " " ) %>% 
   separate(SoilTemp2,into=c("SoilTemp2", "X4"),sep = " " )  %>% 
   mutate(Date = dmy(Date)) %>% 
   select(1,2,4,6,8,10) %>%
   rename(date=Date,
          environment="Environment") %>% 
   mutate(AirTemp1=as.numeric(AirTemp1),AirTemp2=as.numeric(AirTemp2),
          SoilTemp1=as.numeric(SoilTemp1),SoilTemp2=as.numeric(SoilTemp2)) 



###############################Temp data ######################################

#Converting temp to long format
Tempdata_long <- tempdata %>% gather(key="Temp",value = Degree_C,AirTemp1,SoilTemp1,AirTemp2,SoilTemp2) %>% 
   filter(Degree_C !="NA")


#mean temperature on each date plus thermal time

tempdata_mean <- Tempdata_long %>%
   filter(Temp=="AirTemp1" | Temp=="AirTemp2") %>% 
   group_by(environment,date) %>%
   summarise(ave_daily_temp=mean(Degree_C))%>%
   mutate("thermaltime"=cumsum(ave_daily_temp))


  
 


#creating two duplicate files, one for observance, one for emergence

Tempdata_emerg <- tempdata_mean%>%
   rename(emerg_date="date", emerg_tt="thermaltime")%>%
   select(1,2,4)

Tempdata_obs<- tempdata_mean %>%
   rename(obs_date="date", obs_tt="thermaltime") %>%
   select(1,2,4)


##############################haundata################################

#creating column of number of days (obs date minus emergence date)
haundata <- mutate(haundata, days=obs_date - emerg_date)

#Exporting to csv

write_csv(haundata, "data/analysed_data/Haun_data_days.csv")



#adding thermal temp to haun score, calculating change tt

haun_temp <- left_join(haundata,Tempdata_obs,
                       by=c("environment"="environment","obs_date"="obs_date") ) %>% 
             left_join(Tempdata_emerg,
                       by=c("environment"="environment","emerg_date"="emerg_date") ) %>%
             mutate(tt_haun=obs_tt-emerg_tt) %>%
             select(1,2,3,6,10)
                     

#Exporting to csv
write_csv(haun_temp, "data/analysed_data/Haun_temp.csv")




########################Final_leaf_data ########################
#transferring final_leaf to separate file, it is in long format



final_leaf <- filter(maindata,final_leaf!="NA") %>% select(1:3,14)

#exporting to csv

write_csv(final_leaf, "data/analysed_data/final_leaf.csv")

###mean final leaf####


mean_final_leaf <- group_by(final_leaf, environment, genotype) %>% 
   
   summarise(mean_final_leaf=mean(final_leaf))

write_csv(mean_final_leaf, "data/analysed_data/mean_final_leaf.csv")




#########################Maindata separated into spikelet and flowering/heading

#remaining data in two new files, all values that are dates in one, spikelet in second


maindata_date <- select(maindata, 1:4,8:13)
maindata_spikelet <- select(maindata, 1:5,15,16)



#########spike data
spikelet_long <- gather(maindata_spikelet,key=type, value=spikelet_no,spk_MS, spk_t1) %>% 
   filter(spikelet_no!="NA") %>% 
   select(1,2,3,6,7) 

write_csv(spikelet_long, "data/analysed_data/spikelet_data.csv")



########maindata with dates, making it longgggggg


maindata_long <-  (gather(maindata_date, key=obs_type, value=obs_date, 
                    hd_MS, hd_t1, half_hd, fwr_MS, fwr_t1, half_fwr) ) %>% 
   mutate(obs_date = ymd(obs_date))


write_csv(maindata_long, "data/analysed_data/Flw__hd_date.csv")

##Flowering and heading with thermal time


fwr_hd_tt <- left_join(maindata_long,Tempdata_obs,
                                  by=c("environment"="environment","obs_date"="obs_date") ) %>% 
                        left_join(Tempdata_emerg,
                                  by=c("environment"="environment","emerg_date"="emerg_date") ) %>%
                        mutate(thermaltime=obs_tt-emerg_tt) %>%
                        select(1,2,3,5,9) %>% filter(thermaltime!="NA")

write_csv(fwr_hd_tt, "data/analysed_data/fwr_hd_tt.csv")


########## all data in one file for Rmarkdown report, wide format (excludes haun)

glimpse(flowering_heading_tt)
glimpse(spikelet_long)
glimpse(final_leaf)
glimpse(haun_temp)


fwr_tt <- fwr_hd_tt %>% 
   filter(obs_type=="fwr_MS" | obs_type=="fwr_t1") %>%
   filter(thermaltime!="NA") %>% rename(flw_tt= 'thermaltime' ) %>% 
   select(1:3, 5)

hd_tt <- fwr_hd_tt %>% 
   filter(obs_type=="hd_MS" | obs_type=="hd_t1") %>%
   filter(thermaltime!="NA") %>% rename(hd_tt= 'thermaltime' ) %>% 
   select(1:3, 5)
   
all_data_wide <-  full_join(fwr_tt, hd_tt,
             by=c("environment"="environment","reps"="reps","genotype"="genotype") ) %>% 
             full_join(final_leaf,
             by=c("environment"="environment","reps"="reps","genotype"="genotype") ) %>% 
             full_join(spikelet_long,
             by=c("environment"="environment","reps"="reps","genotype"="genotype") ) %>% select (1:6, 8)


write_csv(all_data_wide, "data/analysed_data/all_data_wide.csv")

#######Advice from Aswin to clear the Environment ###########################
#How to clear the environment
ls()%in%(haun_temp)
!ls()%in%('haun_temp')
ls()[!ls()%in%('haun_temp')]

LETTERS[1:8]

keep_objects <- c("haun_temp")
rm(ls()[!ls()%in% keep_objects])
rm(list=ls()[!ls()%in% ("haun_temp")])
