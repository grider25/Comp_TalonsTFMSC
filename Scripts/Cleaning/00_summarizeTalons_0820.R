##############
#
#
#
#
#
###############

library(itRePACKage)
library(tidyverse)
getDrivePath('01_EconPolicy_Aviation', '01_Misc Data and NCDoA Asks/9999 Talons_TFMSC', 'gdrider')->drivePath

inputPath <- '/05 - Data/ncTalons2024/'

list.files(paste0(drivePath, inputPath), pattern='talons')->talonsFiles


summarize_talons <- function(index){
read.csv(paste0(drivePath, inputPath, talonsFiles[index]))->ds

stringr::str_split_fixed(ds$day, '\\-', 3) -> separateYear
aptname <- str_sub(talonsFiles[index], 7, 9)

ds2 <- ds %>% 
  mutate(
    year =separateYear[,1],
    month = separateYear[,2],
    day = separateYear[,3]
    ) %>% 
  select(-day)


talonsByMonth <-  ds2 %>% 
    group_by(month) %>% 
    summarize(landings=sum(landings),
              takeoffs=sum(takeoffs),
              go_arounds = sum(go_arounds),
              overflights = sum(overflights),
              avg_wind_speed = mean(na.omit(avg_wind_speed)),
              avg_temp = mean(na.omit(avg_temp))) %>% 
    mutate(airport = aptname)
 
return(talonsByMonth) 
}


for(i in 1:10){
  print(summarize_talons(i))
}
