#Load packages

library(tidyverse)
library(cowplot)


#set working directory

  setwd("C:/Users/rat05a/DATA School FOCUS/Data_school_report/")

#Upload files 
haundays <- read_csv("data/analysed_data/Haun_data_days.csv")
haun_thermal_time <- read_csv("data/analysed_data/Haun_temp.csv")
final_leaf <- read_csv("data/analysed_data/final_leaf.csv")
spikelet_data <- read_csv("data/analysed_data/spikelet_data.csv") 
flw_hd_data <- read_csv("data/analysed_data/fwr_hd_tt.csv")
mean_final_leaf <- read_csv("data/analysed_data/mean_final_leaf.csv")
all_data_wide <- read_csv("data/analysed_data/all_data_wide.csv")
#dotplot of date v haun score

haundays %>% 
  ggplot(mapping=aes(x=obs_date, y=haun, colour=environment))+
  geom_point()



haundays %>% 
  ggplot(mapping=aes(x=days, y=haun, colour=environment))+
  geom_point(alpha=0.5, size=1)


haun_thermal_time %>% ggplot(mapping=aes(x=tt_haun, y=haun, colour=environment))+
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

final_leaf %>% ggplot(mapping=aes(x=final_leaf, fill=environment))+
  geom_histogram()+
  facet_wrap(~environment)

final_leaf %>% ggplot(mapping=aes(x=final_leaf, fill=environment))+
  geom_histogram()+
  facet_wrap(~genotype)

final_leaf %>% ggplot(mapping=aes(x=final_leaf, colour=environment))+
  geom_freqpoly(binwidth=3)

final_leaf %>% ggplot(mapping=aes(x=final_leaf, colour=environment))+
  geom_freqpoly(binwidth=2)+
  facet_wrap(~genotype)

final_leaf %>% 
  ggplot( mapping = aes(x = final_leaf, fill=environment)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~environment)

final_leaf %>%
  ggplot( mapping = aes(x = final_leaf, fill=environment)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~genotype)

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
spikelet_data %>% ggplot(mapping=aes(x=environment, y=spikelet_no))+
  geom_boxplot()+
  facet_wrap(~genotype)

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

all_data_vern <- all_data_wide %>% filter(genotype=="AXE"| genotype=="EMU_ROCK"|
                                    genotype=="KITTYHAWK" | genotype=="EGA_WEDGETAIL"|
                                    genotype=="CSIROW087"| genotype=="CSIROW007", 
                                    environment=="LV" | environment=="LN")

#flowering plot
 all_data_vern %>% filter(obs_type=="fwr_MS") %>% 
  ggplot(mapping=aes(x=environment, y=thermaltime, colour=obs_type))+
  geom_point()+
  facet_wrap(~genotype)

#heading plot
all_data_vern %>% 
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
  
 ##### Flowering plots
  
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    ggplot(mapping=aes(x=genotype, y=thermaltime, colour=environment))+
    geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    ggplot(mapping=aes(x=genotype(genotype, thermaltime), y=thermaltime, colour=environment))+
    geom_boxplot()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
  
  
  flw_hd_data %>% 
    filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    group_by(genotype, environment) %>% 
    summarise(mean_fwr = mean(thermaltime)) %>% 
    ggplot(mapping=aes(x=genotype, y=mean_fwr, colour=environment))+
    geom_point()+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  flw_hd_data %>% 
    filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>%
    filter(genotype=="AXE"| genotype=="ELLISON"|
              genotype=="BEAUFORT" | genotype=="EGA_EAGLEHAWK"|
             genotype=="KITTYHAWK"| genotype=="EMU_ROCK"|
           genotype=="YITPI"| genotype=="EGA_WEDGETAIL"|
              genotype=="CSIROW029"| genotype=="CSIROW087"|
           genotype=="CSIROW007"| genotype=="CSIROW005") %>% 
    group_by(genotype, environment) %>% 
    summarise(mean_fwr = mean(thermaltime)) %>% 
    ggplot(mapping=aes(x=genotype, y=mean_fwr, colour=environment))+
    geom_point()+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  chosen_few <- flw_hd_data %>% 
    filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>%
    filter(genotype=="AXE"| genotype=="ELLISON"|
             genotype=="BEAUFORT" | genotype=="EGA_EAGLEHAWK"|
             genotype=="KITTYHAWK"| genotype=="EMU_ROCK"|
             genotype=="SUNTOP"| genotype=="EGA_WEDGETAIL"|
             genotype=="CSIROW029"| genotype=="CSIROW087"|
             genotype=="CSIROW007"| genotype=="CSIROW005") %>% 
    spread(key="environment", value="thermaltime") 
  
  chosen_few %>% 
    ggplot(data=chosen_few,mapping=aes(x=genotype, y=LN))+
    geom_point(colour="blue")+
    geom_point(data=chosen_few, mapping=aes(x=genotype, y=LV), colour="red")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  chosen_few %>% 
    ggplot(data=chosen_few,mapping=aes(x=genotype, y=LN))+
    geom_boxplot(colour="purple")+
    geom_boxplot(data=chosen_few, mapping=aes(x=genotype, y=LV), colour="red")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  chosen_few %>% 
    ggplot(data=chosen_few,mapping=aes(x=genotype, y=SV))+
    geom_boxplot(colour="blue")+
    geom_boxplot(data=chosen_few, mapping=aes(x=genotype, y=LV), colour="red")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  chosen_few %>% 
    ggplot(data=chosen_few,mapping=aes(x=genotype, y=SV))+
    geom_point(colour="blue")+
    geom_point(data=chosen_few, mapping=aes(x=genotype, y=LV), colour="red")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  flw_hd_data %>% 
    filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>%
    spread(key="environment", value="thermaltime") %>% 
    ggplot(mapping=aes(x=LV, y=LN))+
    geom_point()+
    geom_smooth(method=lm)
  
  flw_hd_data %>% 
    filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>%
    spread(key="environment", value="thermaltime") %>% 
    ggplot(mapping=aes(x=LV, y=SV))+
    geom_point()+
    geom_smooth(method=lm) 
  
  
  
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    ggplot(mapping=aes(x=genotype, y=thermaltime, colour=environment))+
    geom_point()
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    ggplot(mapping=aes(x=genotype, y=thermaltime, colour=environment))+
    geom_boxplot()
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LN!="NA")  %>% 
    filter( LV!="NA") %>% 
    mutate(vern_effect=LN/LV) %>%
    filter(vern_effect>1.5) %>% 
    ggplot(mapping=aes(x=genotype, y=vern_effect))+
    geom_boxplot(colour="blue")
  
 #### Need to change the scales so they are all the same
flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LN!="NA")  %>% 
    filter( LV!="NA") %>% 
    group_by(genotype) %>% 
    mutate(vern_effect=mean(LN/LV)) %>%
    filter(vern_effect>1.5) %>% 
    ggplot(mapping=aes(x=genotype, y=LN))+
    geom_boxplot(colour="red")+
  theme(axis.text.x = element_text(angle = 90, size = 8))
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LN!="NA")  %>% 
    filter( LV!="NA") %>% 
    group_by(genotype) %>% 
    mutate(vern_effect=mean(LN/LV)) %>%
    filter(vern_effect>1.5) %>% 
    ggplot(mapping=aes(x=genotype, y=LN))+
    geom_boxplot(colour="blue")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  
  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LN!="NA")  %>% 
    filter( LV!="NA") %>% 
    group_by(genotype) %>% 
    mutate(vern_effect=mean(LN/LV)) %>%
    filter(vern_effect<1.5) %>% 
    ggplot(mapping=aes(x=genotype, y=LN))+
    geom_boxplot(colour="purple")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  roughplot4 <- flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="LV"|environment=="LN" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LN!="NA")  %>% 
    filter( LV!="NA") %>% 
    group_by(genotype) %>% 
    mutate(vern_effect=mean(LN/LV)) %>%
    filter(vern_effect<1.5) %>% 
    ggplot(mapping=aes(x=genotype, y=LN))+
    geom_boxplot(colour="green")+
    theme(axis.text.x = element_text(angle = 90, size = 8))
  
  
    #############################VERN graph ordered############################################
  
  flw_hd_data_vern <-  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
      filter(environment=="LV"|environment=="LN" ) %>% 
      group_by(genotype, reps, environment) %>% 
      spread(key=environment,value=thermaltime) %>% 
      filter(LN!="NA")  %>% 
      filter( LV!="NA") 
  
  flw_hd_data_pp <-  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
    filter(environment=="SV"|environment=="LV" ) %>% 
    group_by(genotype, reps, environment) %>% 
    spread(key=environment,value=thermaltime) %>% 
    filter(LV!="NA")  %>% 
    filter( SV!="NA") 
  
  flw_vern_subset <-flw_hd_data_vern %>% 
    filter( genotype=="AXE" |genotype=="FORREST"|genotype=="LONGSWORD"|genotype=="MANNING")
  
      ggplot(data=flw_hd_data_vern,mapping=aes(x=reorder(genotype, LN), y=LN))+
    geom_boxplot(colour="green")+
        geom_boxplot(data=flw_hd_data_vern,mapping=aes(x=reorder(genotype, LN),y=LV),colour="blue")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
      
      ggplot(data=flw_hd_data_vern,mapping=aes(x=reorder(genotype, LN), y=LN))+
        geom_point(colour="green")+
       geom_point(data=flw_hd_data_vern,mapping=aes(x=reorder(genotype, LN),y=LV),colour="blue")+
      geom_point(data=flw_vern_subset, mapping=aes(x=reorder(genotype, LN), y=LN), colour="green")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
      
      
      #############################PP graph ordered############################################
      
      flw_hd_data_pp <-  flw_hd_data %>% filter(obs_type=="fwr_MS" |obs_type=="fwr_t1") %>% 
        filter(environment=="SV"|environment=="LV" ) %>% 
        group_by(genotype, reps, environment) %>% 
        spread(key=environment,value=thermaltime) %>% 
        filter(LV!="NA")  %>% 
        filter( SV!="NA") 
      
      flw_pp_subset <-flw_hd_data_pp %>% 
        filter( genotype=="AXE" |genotype=="FORREST"|genotype=="LONGSWORD"|genotype=="MANNING")
      
      ggplot(data=flw_hd_data_pp,mapping=aes(x=reorder(genotype, SV), y=LV))+
        geom_boxplot(colour="blue")+
        geom_boxplot(data=flw_hd_data_pp,mapping=aes(x=reorder(genotype, SV), y=SV), colour="purple")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
      
      ggplot(data=flw_hd_data_pp,mapping=aes(x=reorder(genotype, SV), y=LV))+
        geom_point(colour="blue")+
        geom_point(data=flw_hd_data_pp,mapping=aes(x=reorder(genotype, SV), y=SV), colour="purple")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
      
      
      #############################flowering v heading############################
  
        