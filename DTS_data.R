####DTS Heatmap Figure ####
#Goal is to get DTS data from wide format to long format. Then make a heat map.

library(tidyverse)
library(lubridate)

####Gather data then seperate date and time values ####

#1. Magic function to set columns to numeric not character
#https://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once
convert.magic <- function(obj,types){
  for (i in 1:length(obj)) {
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

#2. Input data
dts <- read.csv("DTS_2_Cleaned_up.csv")

dtslong <- dts %>%
  gather(dt,temp,-long,-long2) %>%
  separate(dt,c("xmonth", "day", "year", "hour", "minute")) %>% 
  mutate(month = 7) %>%                                              #cleans up xmonth to really just be month which is july (7)
  select(long, long2, month, day, year, hour, minute, temp)          # str(dtslong) - date is chr not num

# Change all columns to numeric, use magic function above
out <- convert.magic(dtslong,c('numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
str(out)

#3. Get DTS data into timestamp form rather then multiple columns
dts_dt <- out  %>% 
  mutate(timestamp = make_datetime(month, day, year, hour, minute)) %>% 
  select(long, long2, timestamp, temp)

####Now make a heatmap in ggplot ####


