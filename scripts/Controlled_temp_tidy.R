

#Load packages

library(tidyverse)
library(readxl)
library(lubridate)
library(modelr)
library(broom)

#set working directory

setwd("C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data")

#Upload files
haundata <- read_xlsx("Rawdata/haundata.xlsx")
tempdata <- read_csv("Rawdata/Soilandtemp_data.csv",skip = 1)
restdata <- read_xlsx("Rawdata/controlled_temp_data_without_haun.xlsx")


###################Fixing column headers and column types#######################

#note MS=mainstem, t1=tiller1, fwr=flowering, spk=Spikelet number, hd=heading


maindata2 <- rename(restdata, 
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

haundata2 <- haundata %>% 
   rename(environment="Environment",
          reps="Reps",
          genotype="Genotype",
          haun="Haun Stage",
          emerg_date="Emergence date", 
          obs_date="Date") %>%
   mutate(obs_date = ymd(obs_date), emerg_date=ymd(emerg_date)) 
   
tempdata2 <- tempdata %>% 
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
Tempdata_long <- tempdata2 %>% gather(key="Temp",value = Degree_C,AirTemp1,SoilTemp1,AirTemp2,SoilTemp2) %>% 
   filter(Degree_C !="NA")


#mean temperature on each date

tempdata_mean <- Tempdata_long %>%
   filter(Temp=="AirTemp1" | Temp=="AirTemp2") %>% 
   group_by(environment,date) %>%
   summarise(ave_daily_temp=mean(Degree_C))


#max and min temp each day
tempdata_min_max <- 
   Tempdata_long %>% 
   filter(Temp=="AirTemp1" | Temp=="AirTemp2") %>% 
   group_by(environment,date) %>%
      summarise(min_daily_temp=min(Degree_C),max_daily_temp=max(Degree_C))
   
   
  
#thermal time
    
Tempdata_thermaltime_mean <- tempdata_mean %>%
   mutate("thermaltime"=cumsum(ave_daily_temp))

Tempdata_thermaltime_mm <- tempdata_min_max %>%
   mutate("thermaltime"=cumsum((min_daily_temp+max_daily_temp)/2))

#creating two duplicate files, one for observance, one for emergence

Tempdata_emerg <- Tempdata_thermaltime_mean%>%
   rename(emerg_date="date", emerg_tt="thermaltime")%>%
   select(1,2,4)

Tempdata_obs<- Tempdata_thermaltime_mean %>%
   rename(obs_date="date", obs_tt="thermaltime") %>%
   select(1,2,4)

Tempdata_emerg2 <- Tempdata_thermaltime_mm%>%
   rename(emerg_date="date", emerg_tt="thermaltime")%>%
   select(1,2,4)

Tempdata_obs2<- Tempdata_thermaltime_mm %>%
   rename(obs_date="date", obs_tt="thermaltime") %>%
   select(1,2,4)

##############################haundata################################

#creating column of number of days (obs date minus emergence date)
haundata3 <- mutate(haundata2, days=obs_date - emerg_date)

#Exporting to csv

write_csv(haundata3, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Haun_data_days.csv")



#adding thermal temp to haun score, calculating change tt

haun_temp <- left_join(haundata2,Tempdata_obs,
                       by=c("environment"="environment","obs_date"="obs_date") ) %>% 
             left_join(Tempdata_emerg,
                       by=c("environment"="environment","emerg_date"="emerg_date") ) %>%
             mutate(tt_haun=obs_tt-emerg_tt) %>%
             select(1,2,3,6,9)
                     
haun_temp2 <- left_join(haundata2,Tempdata_obs,
                       by=c("environment"="environment","obs_date"="obs_date") ) %>% 
   left_join(Tempdata_emerg,
             by=c("environment"="environment","emerg_date"="emerg_date") ) %>%
   mutate(tt_haun=obs_tt-emerg_tt) %>%
   select(1,2,3,6,9)
#Exporting to csv
write_csv(haun_temp, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Haun_thermal_time.csv")

write_csv(haun_temp2, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Haun_thermal_time_MaxMin.csv")


########################Final_leaf_data ########################
#transferring final_leaf to separate file, it is in long format



final_leaf <- filter(maindata2,final_leaf!="NA") %>% select(1:3,14)

#exporting to csv

write_csv(final_leaf, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Final_leaf.csv")

###mean final leaf####


mean_final_leaf <- group_by(final_leaf, environment, genotype) %>% 
   
   summarise(mean_final_leaf=mean(final_leaf))

write_csv(mean_final_leaf, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Mean_Final_leaf.csv")




#########################Maindata separated into spikelet and flowering/heading

#remaining data in two new file, all values that are dates in one, spikelet in second


maindata_date <- select(maindata2, 1:4,8:13)
maindata_spikelet <- select(maindata2, 1:5,15,16)



#########spike data
spikelet_long <- gather(maindata_spikelet,key=type, value=spikelet_no,spk_MS, spk_t1) %>% 
   filter(spikelet_no!="NA") %>% 
   select(1,2,3,6,7) 

write_csv(spikelet_long, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Spikelet_data.csv")



########maindata with dates, making it longgggggg


maindata_long <-  (gather(maindata_date, key=obs_type, value=obs_date, 
                    hd_MS, hd_t1, half_hd, fwr_MS, fwr_t1, half_fwr) ) %>% 
   mutate(obs_date = ymd(obs_date))


write_csv(maindata_long, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Floweringdate_and_headingdate_data.csv")

##Flowering and heading with thermal time
glimpse(maindata_long)
glimpse(Tempdata_emerg)
glimpse(Tempdata_obs)

flowering_heading_tt <- left_join(maindata_long,Tempdata_obs,
                                  by=c("environment"="environment","obs_date"="obs_date") ) %>% 
                        left_join(Tempdata_emerg,
                                  by=c("environment"="environment","emerg_date"="emerg_date") ) %>%
                        mutate(thermaltime=obs_tt-emerg_tt) %>%
                        select(1,2,3,5,9) %>% filter(thermaltime!="NA")

write_csv(flowering_heading_tt, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Floweringdate_and_headingdate_tt_data.csv")


########## one file, is this useful?#######no haun data, can add slope in later####

glimpse(flowering_heading_tt)
glimpse(spikelet_long)
glimpse(final_leaf)
glimpse(haun_temp)

all_data <-  full_join(flowering_heading_tt, spikelet_long,
             by=c("environment"="environment","reps"="reps","genotype"="genotype") ) %>% 
             full_join(final_leaf,
             by=c("environment"="environment","reps"="reps","genotype"="genotype") ) 

write_csv(all_data, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/all_data.csv")

#######Advice from Aswin to clear the Environment ###########################
#How to clear the environment
ls()%in%(haun_temp)
!ls()%in%('haun_temp')
ls()[!ls()%in%('haun_temp')]

LETTERS[1:8]

keep_objects <- c("haun_temp")
rm(ls()[!ls()%in% keep_objects])
rm(list=ls()[!ls()%in% ("haun_temp")])
