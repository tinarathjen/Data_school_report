#Load packages

library(tidyverse)
library(readxl)
library(lubridate)

#set working directory



#Upload files 
final_leaf <- read_csv("data/analysed_data/final_leaf.csv")
haun_tt <- read_csv("data/analysed_data/Haun_temp.csv")
spikelet_data <- read_csv("data/analysed_data/spikelet_data.csv")
flw_hd_data <- read_csv("data/analysed_data/fwr_hd_tt.csv")
all_data <- read_csv("data/analysed_data/all_data_wide.csv")
mean_final_leaf <- read_csv("data/analysed_data/mean_final_leaf.csv")
final_leaf_tt <- read_csv("data/analysed_data/final_leaf_tt.csv")

#Parameters
parameter_list <- data.frame("Parameter"=c("Parameter_1","Parameter_2","Parameter_3","Parameter_4","Parameter_5","Parameter_6","Parameter_7"),
                             "Name"=c("Minimal_Leaf_Number","Pp_Sensitivity","Vrn_Sensitivity","Base_Phyllochron",
                                      "Phyllochron_Photoperiod_effect","Early_Reproductive_Long_Day_Base", "Early_Reproductive_Pp_Sensitivity"),
                             "Calculation"=c("LV mean final leaf number","SV mean final leaf number minus LV mean final leaf number",
                                             "LN mean final leaf number minus LV mean final leaf number",
                                             "slope of accummulated thermal time against mean leaf number between 3 and 7 in LV",
                                             "ratio of Phyllochron SV to Phyllochron LV","accumulated thermal time at flowering divided by BasePhyllochron",
                                             "difference of accumulated time between LV and SV treatment divided by BasePhyllochron"))



#####################Parameter 1 #######################

# MinimumLeafNumber defined as final leaf number LV Environment

Parameter_1 <- filter(final_leaf, environment=="LV") %>% 
  group_by(genotype) %>% 
  summarise(Minimum_leaf_number=mean(final_leaf))

write_csv(Parameter_1,"data/Analysed_data/Parameter_1.csv")




###############Parameter 2#################################  

#PPsensitivity defined as difference in leaf number between LV and SV

Ppsensitivity <- filter(final_leaf,environment=="LV"  |environment=="SV")%>% 
  spread(environment,value=final_leaf) %>% 
  rename("LV_final_leaf"="LV", "SV_final_leaf"="SV") %>% 
  filter(LV_final_leaf!="NA") %>% 
  filter(SV_final_leaf!="NA") %>% 
  mutate(Ppsensitivity=SV_final_leaf - LV_final_leaf)%>%
  group_by(genotype) %>%
  summarise(Ppsensitivity=mean(Ppsensitivity)) %>% 
  mutate(Ppsensitivity=ifelse(Ppsensitivity>0,Ppsensitivity,0))




write_csv(Parameter_2,"data/analysed_data/PpSensitivity_parameter.csv")



######################Parameter 3############################

#VrnSensitivity defined as differnece between final leaf number LV and LN


Parameter_3 <- filter(final_leaf,environment=="LV"  |environment=="LN") %>% 
  spread(environment,value=final_leaf) %>% 
  rename("LV_final_leaf"="LV", "LN_final_leaf"="LN") %>% 
  filter(LN_final_leaf!="NA") %>%  
  mutate(vrnsensitivity= LN_final_leaf - LV_final_leaf)%>%
  group_by(genotype) %>% 
  summarise(Vrn_sensitivity=mean(vrnsensitivity)) 


write_csv(Parameter_3,"data/analysed_data/Parameter_3.csv")


##########################Parameter_4##############################################

Phyllochron <-  haun_tt %>% filter(haun>3, haun<7) %>% 
  group_by(genotype, environment) %>% 
  do((lm(tt_haun ~ haun, data= .)) %>% 
       coef %>% 
       as_data_frame()) %>% 
  mutate(sel=1:n()) %>%
  filter(sel==2) %>%
  rename(phyllochron=value) %>% 
  select(1:3)

write_csv(Phyllochron, "data/analysed_data/Phyllochron.csv")

Parameter_4 <- Phyllochron %>% 
  filter(environment=="LV") %>% 
  rename(Base_Phyllochron="phyllochron")

write_csv(Parameter_4, "data/analysed_data/Parameter_4.csv")



  ############parameter 5###################
  
  Parameter_5 <- Phyllochron %>% 
  filter(environment=="LV" | environment=="SV") %>% 
  spread(environment, phyllochron) %>% 
  summarise(Phyllochron_PhotoPeriod_Effect=SV/LV)
  
write_csv(Parameter_5, "data/analysed_data/Parameter_5.csv")


##########parameter 6######################
#currently this is not correct

#not thermal time to flowering but thermal time from flag leaf to flowering


  


  flw_half <- flw_hd_data %>% 
        filter(obs_type=="half_fwr")
  
tt_flag_flower <- flw_half %>% 
  full_join(final_leaf_tt,by=c("genotype", "environment", "reps")) %>% 
    filter(environment=="LV") %>% 
  mutate(tt_flag_flower=thermaltime -tt_final_leaf) %>% 
  group_by(genotype) %>% 
  summarise(mean_tt_ff=mean(tt_flag_flower))
 
  
  Parameter_6 <- tt_flag_flower%>% 
    full_join(Parameter_4, by=c("genotype")) %>% 
   mutate(Early_Reproductive_Long_Day_Base=mean_tt_ff / Base_Phyllochron) %>% 
    select(1,5)
  

write_csv(Parameter_6, "data/analysed_data/Parameter_6.csv")

###Parameter7#############

tt_flag_flower_pp <- flw_half %>% 
  full_join(final_leaf_tt,by=c("genotype", "environment", "reps")) %>% 
  filter(environment=="LV"| environment=="SV") %>% 
  mutate(tt_flag_flower=thermaltime -tt_final_leaf) %>%
  select(1,2,3,8) %>% 
  spread(environment,tt_flag_flower) %>% 
  mutate(tt_flag_flower_pp=SV -LV)%>% 
    filter(tt_flag_flower_pp!="NA") %>% 
  group_by(genotype) %>% 
  summarise(mean_tt_ff_pp=mean(tt_flag_flower_pp))
  
Parameter_7 <- tt_flag_flower_pp%>% 
  full_join(Parameter_4, by=c("genotype")) %>% 
  mutate(EarlyReproductivePpSensitivity=mean_tt_ff_pp / Base_Phyllochron) %>% 
  select(1,5)

write_csv(Parameter_7, "data/analysed_data/Parameter_7.csv")

#############################################################################
##############################PLOTS##########################################
#############################################################################


#########MinLeaf Parameter plots######
min_leaf <- read_csv("data/analysed_data/Parameter_1.csv")

ggplot(data = min_leaf, mapping = aes(x = mean_final_leaf)) +
  geom_histogram(binwidth = 0.5) 

ggplot(data = min_leaf, mapping = aes(x = mean_final_leaf)) +
  geom_freqpoly(binwidth=0.4) 


min_leaf_a <- arrange(min_leaf, mean_final_leaf)
ggplot(data=min_leaf_a, mapping=aes(x = genotype, y=mean_final_leaf))+
  geom_point(size=2, colour="blue")


ggplot(data=min_leaf, mapping=aes(x = reorder(Genotype,
                                              mean_final_leaf), y=mean_final_leaf))+
  geom_point(size=2, colour="blue")


############Ppsensitivity Parameter plots###########

PpSens <- read_csv("Analysed_data/PpSensitivity_parameter.csv")

ggplot(data = PpSens, mapping = aes(x = mean_Ppsensitivity)) +
  geom_histogram(binwidth = 0.1) 

ggplot(data = PpSens, mapping = aes(x = mean_Ppsensitivity)) +
  geom_freqpoly(binwidth=0.4) 
