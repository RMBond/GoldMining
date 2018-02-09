####DTS Heatmap Figure ####
#Goal is to get DTS data from wide format to long format. Then make a heat map.

library(tidyverse)
library(lubridate)
library(viridis) #install.packages("viridis")

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
  mutate(month = 7, year = 2012) %>%                                              #cleans up xmonth to really just be month which is july (7)
  select(long, long2, year, month, day, year, hour, minute, temp)          # str(dtslong) - date is chr not num

# Change all columns to numeric, use magic function above
out <- convert.magic(dtslong,c('numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'numeric'))
str(out)

#3. Get DTS data into timestamp form rather then multiple columns
dts_dt <- out  %>% 
  mutate(timestamp = make_datetime(year, month, day, hour, minute)) %>% 
  select(long, long2, timestamp, temp)

dts_hour <- out %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select(long, long2, date, hour, temp) %>% 
  group_by(long, date, hour) %>% 
  summarise(meantemp = mean(temp))

str(dts_hour)

####Now make a heatmap in ggplot ####
# 1m pixels (all data)
ggplot(dts_dt, aes(long,timestamp, fill = temp)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_viridis()

# Hourly pixles 
ggplot(dts_hour, aes(long,date, fill = meantemp)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_viridis()  


#+ scale_colour_gradient(low = "#01665e", high = "#8c510a",guide = "colourbar")
#scale_color_continuous("#01665e","#f5f5f5","#8c510a")

