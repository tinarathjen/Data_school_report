#Load packages

library(tidyverse)
library(readxl)
library(lubridate)

#set working directory

setwd("C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data")

#Upload files 
final_leaf <- read_csv("Analysed_data/Final_leaf.csv")
haundays <- read_csv("Analysed_data/Haun_data_days.csv")
haun_tt <- read_csv("Analysed_data/Haun_thermal_time.csv")
spikelet_data <- read_csv("Analysed_data/Spikelet_data.csv")
flw_hd_data <- read_csv("Analysed_data/Floweringdate_and_headingdate_tt_data.csv")
all_data <- read_csv("Analysed_data/all_data.csv")
mean_final_leaf <- read_csv("Analysed_data/Mean_Final_leaf.csv")
haun_tt_mm <- read_csv("Analysed_data/Haun_thermal_time_MaxMin.csv")





#####################Parameter 1 #######################

# MinimumLeafNumber defined as final leaf number LV Environment

Minimum_leaf_number <- filter(final_leaf, environment=="LV") %>% group_by(genotype) %>% summarise(mean_final_leaf=mean(final_leaf))

write_csv(Minimum_leaf_number,"C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/Minimum_leaf_number_parameter.csv")




###############Parameter2#################################  

#PPsensitivity defined as difference in leaf number between LV and SV

Ppsensitivity <- filter(final_leaf,environment=="LV"  |environment=="SV")%>% 
  spread(environment,value=final_leaf) %>% 
  rename("LV_final_leaf"="LV", "SV_final_leaf"="SV") %>% 
  filter(LV_final_leaf!="NA") %>% 
  filter(SV_final_leaf!="NA") %>% 
  mutate(Ppsensitivity=SV_final_leaf - LV_final_leaf)

Ppsensitivity2 <- Ppsensitivity %>%
  group_by(genotype) %>%
  summarise(mean_Ppsensitivity=mean(Ppsensitivity)) %>% 
  mutate(mean_Ppsensitivity=ifelse(mean_Ppsensitivity>0,mean_Ppsensitivity,0))




write_csv(Ppsensitivity2,"C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/PpSensitivity_parameter.csv")



######################Parameter3############################

#VrnSensitivity defined as differnece between final leaf number LV and LN


Vrnsensitivity <- filter(final_leaf,environment=="LV"  |environment=="LN") %>% 
  spread(environment,value=final_leaf) %>% 
  rename("LV_final_leaf"="LV", "LN_final_leaf"="LN") %>% 
  filter(LN_final_leaf!="NA") %>%  
  mutate(vrnsensitivity= LN_final_leaf - LV_final_leaf)

Vrnsensitivity2 <- Vrnsensitivity %>%
  group_by(genotype) %>% 
  summarise(mean_vrnsensitivity=mean(Vrnsensitivity)) 


write_csv(Vrnsensitivity2,"C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/VrnSensitivity_parameter.csv")


##########################Phyllochron##############################################

########How to work out slope####3 using model and co-efficients ##EXAMPLE###
##x <- sample(1:100, 100, replace = TRUE)

##y <- x + rnorm(100, sd = 4)
##mydf <- data.frame(x = x, y = y)
##plot(y ~ x, data = mydf)
##model <- lm(y ~ x, data = mydf)
##abline(model, col = "red")
##summary(model)

#In the Coefficients
#  > coef(model)

#(Intercept)           x
#1.2093273   0.9786051

#Where “(Intercept)” is, well, the y-intercept, and “x” is the slope. In other words, you can retrieve the equation like this:
#paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])



#modelling slope for each genotype separately (Haun between 3 AND 7)

haun_SPITFIRE <- haun_tt %>% 
  filter(haun>3, haun<7, environment=="LV", genotype=="SPITFIRE")
plot(tt_haun ~ haun, data = haun_SPITFIRE)
model <- lm(tt_haun ~ haun, data = haun_SPITFIRE)

abline(model, col = "red")
summary(model)

paste('y =', coef(model)[[2]])

haun_CSIROW029_slope <- haun_CSIROW029 %>% 
  mutate(intercept = coef(model)[[2]])

cor.test(haun_CSIROW029$tt_haun, haun_CSIROW029$haun)

#now with nesting.....

nest_haun_tt_genotype <-  haun_tt %>% filter(haun>3, haun<7) %>% 
  group_by(genotype, environment) %>% 
  nest()

haun_model <- function(df) {lm(tt_haun ~ haun, data = df)}


model <- map(nest_haun_tt_genotype$data, haun_model)


model_map2 <- nest_haun_tt_genotype %>% mutate(model=map(data, haun_model))#works

#worked out with Aswin
group_model <-  haun_tt %>% filter(haun>3, haun<7) %>% 
  group_by(genotype, environment) %>% 
  do((lm(tt_haun ~ haun, data= .)) %>% 
       coef %>% 
       as_data_frame) %>% 
  mutate(sel=1:n()) %>%
  filter(sel==2)


group_model2 <-  haun_tt %>% filter(haun>3, haun<7) %>% 
  group_by(genotype, environment) %>% 
  do((lm(tt_haun ~ haun, data= .)) %>% 
       coef %>% 
       as_data_frame()) %>% 
  mutate(sel=1:n()) %>%
  filter(sel==2) %>%
  rename(phyllochron=value) %>% 
  select(1:3)

write_csv(group_model2, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/haun_parameters.csv")

#using maxmin file
group_model_mm <-  haun_tt_mm %>% filter(haun>3, haun<7) %>% 
  group_by(genotype, environment) %>% 
  do((lm(tt_haun ~ haun, data= .)) %>% 
       coef %>% 
       as_data_frame()) %>% 
  mutate(sel=1:n()) %>%
  filter(sel==2) %>%
  rename(phyllochron=value) %>% 
  select(1:3)

write_csv(group_model_mm, "C:/Users/rat05a/DATA School FOCUS/Controlled_temp_data/Analysed_data/haun_parameters_mm.csv")




model_map3 <- nest_haun_tt_genotype %>% mutate(n=pmap(list(data, haun_model, data), list))





###this works kinda but gives a different result? for the ones i checked
haun_temp_coef <- haun_tt %>% 
  group_by(environment, genotype) %>% 
  do(data.frame(., as.list(coef(lm(tt_haun ~ haun, data = .))))) %>%
  rename_at(11:12, ~c( "coef1", "coef2"))



#only the coefficients
lm_coef <- function(df) {coef(lm(tt_haun ~ haun, data = df))}
model_map_coef <- nest_haun_tt_genotype %>% mutate(model=map(data, lm_coef))


##trying to replicate Zhigan's numbers
##Only first six dates, using haun_tt based on means

haun_LV_3_7 <- haundays %>% 
  filter(environment=="LV") %>% 
  filter(obs_date<15-01-2019)
  filter(haun>3 | haun<7)

#############################################################################
##############################PLOTS##########################################
#############################################################################


#########MinLeaf Parameter plots######
min_leaf <- read_csv("Analysed_data/Minimum_leaf_number_parameter.csv")

ggplot(data = min_leaf, mapping = aes(x = mean_final_leaf)) +
  geom_histogram(binwidth = 0.5) 

ggplot(data = min_leaf, mapping = aes(x = mean_final_leaf)) +
  geom_freqpoly(binwidth=0.4) 


min_leaf_a <- arrange(min_leaf, mean_final_leaf)
ggplot(data=min_leaf_a, mapping=aes(x = Genotype, y=mean_final_leaf))+
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
