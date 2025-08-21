#####
#
#
#
######

library(tidyverse)
itRePACKage::getDrivePath('01_EconPolicy_Aviation', '01_Misc Data and NCDoA Asks/9999 Talons_TFMSC', 'gdrider')->drivePath
inputPath <- '/05 - Data/'
outputPath <- './06 - Tasks and Analysis/01_R Output/'

summarizeTFMSC_withJets <- function(projectPath, inputPath, fileName){
read.csv(paste0(projectPath, inputPath, fileName), skip=3) ->ds

ds <- (ds[,-1])

#change column names
colnames(ds) <- c("airport", 
                         "date", 
                         'is_rjet',
                         "is_bjet",
                         "departures",
                         'arrivals', 
                         'total_ops', 
                         'd_seats', 
                         'avg_d_seats', 
                         'a_seats', 
                         'avg_a_seats')


foo <- ds %>% 
  filter(airport != '') %>% 
  select(airport,
         date,
         is_rjet,
         is_bjet, 
         total_ops) %>% 
  mutate(month = stringr::str_split_i(date, '\\-',1),
         year = stringr::str_split_i(date, '\\-', 2),
         total_ops = as.numeric(total_ops)) %>% 
  select(airport, month, year, is_rjet, is_bjet, total_ops)

monthlyOps_byAirport <- foo %>%
  group_by(airport, month) %>% 
  summarize(ops = sum(total_ops)) 

annualOps_byAirport <- foo %>% 
  group_by(airport) %>% 
  summarize(ops=sum(total_ops))

return(list(monthlyOps_byAirport, annualOps_byAirport))
}

summarizeTFMSC_withJets(drivePath, inputPath, '2024NCGAA_TFMSC.csv')-> summary_2024

summary_2024[[1]]
