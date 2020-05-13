#Load packages

library(tidyverse)
library(cowplot)


#set working directory

setwd("C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data")

#Upload files 
haundays <- read_csv("Analysed_data/Haun_data_days.csv")
haun_thermal_time <- read_csv("Analysed_data/Haun_thermal_time.csv")
final_leaf <- read_csv("Analysed_data/Final_leaf.csv")
spikelet_data <- read_csv("Analysed_data/Spikelet_data.csv") 
flw_hd_data <- read_csv("Analysed_data/Floweringdate_and_headingdate_tt_data.csv")
mean_final_leaf <- read_csv("Analysed_data/Mean_Final_leaf.csv")
all_data <- read_csv("Analysed_data/all_data.csv")%>% 
  rename(spikelet_no="no._spikelets")
#dotplot of date v haun score

haun_stage_v_date_roughplot <- haundays %>% ggplot(mapping=aes(x=obs_date, y=haun, colour=environment))+
  geom_point()

haun_stage_v_days_roughplot <-haundays %>% ggplot(mapping=aes(x=days, y=haun, colour=environment))+
  geom_point(alpha=0.5, size=1)


haundays %>%  filter(environment=="SN")%>% 
  group_by(genotype,days) %>%
  summarise(mean_haun=mean(haun))%>% 
  ggplot(mapping=aes(x=days, y=mean_haun))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)


  

#dotplot of thermal time v haun score
haun_thermal_time %>%  
  ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

#plot with separate environments
haun_thermal_time %>%  
  filter(environment=="SN") %>%
  ggplot(mapping=aes(x=tt_haun, y=haun ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

haun_thermal_time %>%  
  filter(environment=="LN") %>%
  ggplot(mapping=aes(x=tt_haun, y=haun ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

haun_thermal_time %>%  
  filter(environment=="SV") %>%
  ggplot(mapping=aes(x=tt_haun, y=haun ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

haun_thermal_time %>%  
  filter(environment=="LV") %>%
  ggplot(mapping=aes(x=tt_haun, y=haun ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

haun_thermal_time %>%
  filter(genotype=="ADV08.0008")%>%
  ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment ))+
  geom_point(alpha=0.5, size=1)+
  geom_smooth(method= "lm", size=1)


haun_thermal_time %>%  
  filter(genotype=="ADV11.9419") %>% 
  ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment ))+
  geom_point(alpha=0.5, size=1)+
  geom_smooth(method= "lm", size=1)

haun_thermal_time %>%
  filter(genotype=="AXE")%>%
  ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment ))+
  geom_point(alpha=0.5, size=1)+
  geom_smooth(method= "lm", size=1)

haun_thermal_time %>% ggplot(mapping=aes(x=tt_haun, y=haun, colour=environment ))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)+
  geom_smooth(method= "lm", size=1)

#once parameters determined then can plot parameters versus genotype



############Final leaf plots##########


final_leaf_PP<- final_leaf %>% filter(environment=="SV" |environment=="LV")

final_leaf_V <- final_leaf %>% filter(environment=="LV" |environment=="LN")

final_leaf %>% ggplot(mapping=aes(x=environment, y=final_leaf))+
  geom_point(alpha=0.5, size=1)

final_leaf %>% ggplot(mapping=aes(x=environment, y=final_leaf))+
  geom_boxplot(alpha=0.5, size=1)

final_leaf %>% ggplot(mapping=aes(x=final_leaf))+
  geom_histogram()+
  facet_wrap(~environment)

final_leaf %>% ggplot(mapping=aes(x=final_leaf))+
  geom_histogram()+
  facet_wrap(~Genotype)

final_leaf %>% ggplot(mapping=aes(x=final_leaf, colour=environment))+
  geom_freqpoly(binwidth=3)

final_leaf %>% ggplot(mapping=aes(x=final_leaf, colour=environment))+
  geom_freqpoly(binwidth=2)+
  facet_wrap(~genotype)

final_leaf %>% 
  ggplot( mapping = aes(x = final_leaf, colour=environment)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~environment)

final_leaf %>%
  ggplot( mapping = aes(x = final_leaf, colour=environment)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Genotype)

final_leaf %>% 
  filter(environment=="SV" |environment=="LV") %>% 
  ggplot(mapping = aes(x = final_leaf, colour=environment)) +
  geom_histogram(binwidth = 1) + facet_wrap(~genotype)

final_leaf %>% 
  filter(environment=="LV" |environment=="LN") %>% 
  ggplot( mapping = aes(x = final_leaf, colour=environment)) +
  geom_histogram(binwidth = 1) + facet_wrap(~genotype)


final_leaf %>% ggplot(mapping=aes(x=environment, y=final_leaf))+
  geom_point(alpha=0.5, size=1)

mean_final_leaf %>% ggplot(mapping=aes(x=environment, y=mean_final_leaf))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)

mean_final_leaf %>% ggplot(mapping=aes(x=environment, y=mean_final_leaf, group=genotype))+
  geom_point()+
  geom_line(aes(colour=genotype), alpha=0.6)

mean_final_leaf %>% ggplot(mapping=aes(x=Environment, y=mean_final_leaf, group=genotype))+
  geom_point()+
  geom_line(aes(colour=genotype), alpha=0.6) +
  facet_wrap(~genotype)

####spike plots######
spikelet_data %>% ggplot(mapping=aes(x=environment, y=spikelet_no, colour=type))+
  geom_point()+facet_wrap(~genotype)

spikelet_data %>% 
  filter(genotype=="AXE") %>% 
    ggplot(mapping=aes(x=environment, y=spikelet_no, colour=type))+
           geom_point()

spikelet_data %>% 
  filter(environment=="LV" |environment=="LN", type=="spk_MS") %>%
  ggplot(mapping=aes(x=environment, y=spikelet_no, colour=type))+
  geom_point()+facet_wrap(~genotype)

spikelet_data %>% 
  filter(environment=="LV" |environment=="SV", type=="spk_MS") %>%
  ggplot(mapping=aes(x=environment, y=spikelet_no, colour=type))+
  geom_point()+facet_wrap(~genotype)

  ############heading  plots##############
  
flw_hd_data %>% filter(obs_type=="hd_MS",environment=="LN" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
    geom_point()+
    facet_wrap(~genotype)

flw_hd_data %>% filter(obs_type=="hd_MS",environment=="SV" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)

flw_hd_data %>% filter(obs_type=="hd_MS",environment=="LN" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)

flw_hd_data %>% filter(obs_type=="hd_MS",environment=="LN" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)


#Flowering plots

flw_hd_data %>% filter(obs_type=="fwr_MS",environment=="SV" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)

flw_hd_data %>% filter(obs_type=="fwr_MS",environment=="LN" | environment=="LV") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)


#Subset representative lines Vernalisation

all_data_vern <- all_data %>% filter(genotype=="AXE"| genotype=="EMU_ROCK"|
                                    genotype=="KITTYHAWK" | genotype=="EGA_WEDGETAIL"|
                                    genotype=="CSIROW087"| genotype=="CSIROW007", 
                                    environment=="LV" | environment=="LN")

#flowering plot
roughplot1 <- all_data_vern %>% filter(obs_type=="fwr_MS") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)

#heading plot
roughplot2 <- all_data_vern %>% 
  filter(obs_type=="hd_MS") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime))+
  geom_point(colour="blue")+
  facet_wrap(~genotype)

#spikelet plot
roughplot3 <- all_data_vern %>% 
  filter(type=="spk_MS") %>% 
  ggplot(mapping=aes(x=environment, y=spikelet_no))+
  geom_point(colour="green")+
  facet_wrap(~genotype)

roughplot4 <- all_data_vern %>% 
  filter(type=="spk_MS") %>% 
  ggplot(mapping=aes(x=environment, y=spikelet_no, colour=genotype))+
  geom_point()

#final leaf plot
roughplot5 <- all_data_vern %>% 
  ggplot(mapping=aes(x=environment, y=final_leaf))+
  geom_point(colour="purple")+
  facet_wrap(~genotype)

#Haun plot

haun_vern <- haun_thermal_time %>% 
  filter(genotype=="AXE"| genotype=="EMU_ROCK"|
           genotype=="CSIROW087" | genotype=="EGA_WEDGETAIL"|
           genotype=="KITTYHAWK"| genotype=="CSIROW007", 
         environment=="LV" | environment=="LN") %>% 
  filter(haun>3, haun<7)
  
roughplot6 <- haun_vern %>% 
ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment))+
  geom_point(alpha=0.5, size=1)+
  facet_wrap(~genotype)+
  geom_smooth(method= "lm", size=1)


  #Subset representative lines Photoperiod
  
  all_data_pp <- all_data %>% 
  filter(genotype=="AXE"| genotype=="ELLISON"|
        genotype=="BEAUFORT" | genotype=="EGA_EAGLEHAWK"|
        genotype=="CSIROW029"| genotype=="CSIROW087", 
        environment=="LV" | environment=="SV")
  
  #flowering plot
  roughplot7 <- all_data_pp %>% filter(obs_type=="fwr_MS") %>% 
    ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
    geom_point()+
    facet_wrap(~genotype)
  
  #heading plot
  roughplot8 <- all_data_pp %>% 
    filter(obs_type=="hd_MS") %>% 
    ggplot(mapping=aes(x=environment, y=thermaltime))+
    geom_point(colour="blue")+
    facet_wrap(~genotype)
  
  #spikelet plot
  roughplot9 <- all_data_pp %>% 
    filter(type=="spk_MS") %>% 
    ggplot(mapping=aes(x=environment, y=spikelet_no))+
    geom_point(colour="green")+
    facet_wrap(~genotype)
  
  roughplot10 <- all_data_pp %>% 
    filter(type=="spk_MS") %>% 
    ggplot(mapping=aes(x=environment, y=spikelet_no, colour=genotype))+
    geom_point()
  
  #final leaf plot
  roughplot11 <- all_data_pp %>% 
    ggplot(mapping=aes(x=environment, y=final_leaf))+
    geom_point(colour="purple")+
    facet_wrap(~genotype)
  #haun
  
  haun_pp <- haun_thermal_time %>% 
    filter(genotype=="AXE"| genotype=="ELLISON"|
              genotype=="BEAUFORT" | genotype=="EGA_EAGLEHAWK"|
              genotype=="CSIROW029"| genotype=="CSIROW087", 
            environment=="LV" | environment=="SV")%>% 
    filter(haun>3, haun<7)
  
  roughplot12 <- haun_pp %>% 
    ggplot(mapping=aes(x=haun, y=tt_haun, colour=environment))+
    geom_point(alpha=0.5, size=1)+
    facet_wrap(~genotype)+
    geom_smooth(method= "lm", size=1)
  
 
  
