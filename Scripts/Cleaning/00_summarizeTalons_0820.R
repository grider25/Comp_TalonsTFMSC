##############
# ITRE Talons/TFMSC comparisons
# Programmed by: Garrett Rider
# Programmed to: clean talons data from 2024
# Program date: 8/20/2025
###############

library(itRePACKage)
library(tidyverse)


getDrivePath('01_EconPolicy_Aviation', '01_Misc Data and NCDoA Asks/9999 Talons_TFMSC', 'gdrider')->drivePath
inputPath <- '/05 - Data/ncTalons2024/'
outputPath <- './06 - Tasks and Analysis/01_R Output/'


#Function to summarize TALONS data for all contributing airports in 1200.aero
summarize_talons <- function(extension){
  
  #read the data in using the paths
  read.csv(paste0(drivePath, inputPath,extension))->ds
  
  #split the date to get year, month, day
  stringr::str_split_fixed(ds$day, '\\-', 3) -> separateYear
  
  #because of how the files are named, this is easy: we can pull the 3-letter code (it's all the same)
  aptname <- str_sub(extension, 7, 9)
  
  #add the components of the dates as separate variables
  ds2 <- ds %>% 
    mutate(
      year =separateYear[,1],
      month = separateYear[,2],
      day = separateYear[,3]
    ) %>% 
    select(-day)
  
  #
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


#all files starting with talons: these are from 1200.aero
list.files(paste0(drivePath, inputPath), pattern='talons')->talonsFiles

#set empty
talons <- NA

#loop through the folder
for(i in 1:length(talonsFiles)){

  #add to dataset 
 talons <- rbind(talons, summarize_talons(talonsFiles[i]))
}
talons <- talons[-1,]
