####DTS Heatmap Figure ####
#Goal is to get DTS data from wide format to long format. Then make a heat map.

library(tidyverse)
library(lubridate)
library(gridExtra)
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
  mutate(tmestamp = make_datetime(year, month, day, hour, minute), 
         abv15_5 = ifelse(temp > 15.5,1,0),
         abv17 = ifelse(temp > 17.0,1,0),
         abv20 = ifelse(temp > 20,1,0),
         abv22 = ifelse(temp > 22,1,0)) %>% 
  select(long, long2, tmestamp, temp,abv15_5, abv17, abv20,abv22)

dts_hour <- out %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select(long, long2, date, hour, temp) %>% 
  group_by(long, date, hour) %>% 
  summarise(meantemp = mean(temp))

str(dts_dt)

texdat <- ymd_hm("2017-07-20 20:09") #used as y coordiantes for annotation label

####Now make a heatmap in ggplot ####
# 1m pixels (all data)
dts_1m <- 
  ggplot(dts_dt, aes(long,tmestamp, fill = temp)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_viridis() +
  labs(x = "Distance downstream [m]", y = "Date") +
  scale_x_continuous(limits = c(1,850)) 
  #scale_y_date(date_labels = "%d")

#1m pixels above 15.5*C
plot15 <- ggplot(dts_dt, aes(long,tmestamp, fill = as.factor(abv15_5))) + 
  geom_tile() +
  theme_classic() +
  labs(x = "Distance downstream [m]", y = "Date") +
  scale_x_continuous(limits = c(1,850)) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray","black")) #, 
  #                   name = "",
  #                   breaks = c(0, 1),
  #                   labels = c(expression(paste("< 15.5",degree,"C")), 
  #                              expression(paste("> 15.5",degree,"C")))) +
  # theme(legend.position = "top") +
  #   annotate("text", label = expression(paste("> 15.5", degree,"C")), #annotation does not work
  #            x = 750, y = texdat, cex = 3, color = "white")

#1m pixels above 17*C
plot17 <- ggplot(dts_dt, aes(long,tmestamp, fill = as.factor(abv17))) + 
  geom_tile() +
  theme_classic() +
  labs(x = "", y = "") +
  #labs(x = "Distance downstream [m]", y = "Date") +
  scale_x_continuous(limits = c(1,850)) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray","black")) #, 
  #                   name = "",
  #                   breaks = c(0, 1),
  #                   labels = c(expression(paste("< 17.0",degree,"C")), 
  #                              expression(paste("> 17.0",degree,"C")))) +
  # theme(legend.position = "top")

#1m pixels above 20*C
plot20 <- ggplot(dts_dt, aes(long,tmestamp, fill = as.factor(abv20))) + 
  geom_tile() +
  theme_classic() +
  labs(x = "", y = "") +
  #labs(x = "Distance downstream [m]", y = "Date") +
  scale_x_continuous(limits = c(1,850)) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray","black")) #, 
  #                   name = "",
  #                   breaks = c(0, 1),
  #                   labels = c(expression(paste("< 20.0",degree,"C")), 
  #                              expression(paste("> 20.0",degree,"C")))) +
  # theme(legend.position = "top")

#1m pixels above 22*C
plot22 <- ggplot(dts_dt, aes(long,tmestamp, fill = as.factor(abv22))) + 
  geom_tile() +
  theme_classic() +
  labs(x = "", y = "") +
  theme(legend.position = "none") + 
  #labs(x = "Distance downstream [m]", y = "Date") +
  scale_x_continuous(limits = c(1,850)) + 
  scale_fill_manual(values = c("gray","black")) #, 
  #                   name = "",
  #                   breaks = c(0, 1),
  #                   labels = c(expression(paste("< 22.0",degree,"C")),
  #                              expression(paste("> 22.0",degree,"C")))) +
  # theme(legend.position = "top")

  ####Muliple temp threshold plots together ####
  
  grid.arrange(plot15,plot17,plot20, plot22, nrow = 2)
  
  
#### Hourly pixles ####
ggplot(dts_hour, aes(long,date, fill = meantemp)) + 
  geom_tile() +
  theme_bw() +
  scale_fill_viridis()  


#+ scale_colour_gradient(low = "#01665e", high = "#8c510a",guide = "colourbar")
#scale_color_continuous("#01665e","#f5f5f5","#8c510a")

#### Hourly mean temperature line plot ####
meantemp <- dts_dt %>% 
  select(long, long2, timestamp, temp) %>% 
  group_by(timestamp) %>% 
  mutate( mtemp = mean(temp)) %>% 
  ungroup(timestamp)  


#temp_mean <- 
  ggplot(meantemp, aes(timestamp, mtemp)) +
  geom_line() + 
  theme_bw()  +
  labs( x = "Date", y = "Mean Temperature [*C]")

#Plot with raw data - NOTE: it will take a while to load.
ggplot(meantemp, aes(timestamp, mtemp)) +
  geom_line() + 
  #geom_point(aes(timestamp, temp)) +
  theme_bw()  +
  labs(x = "Date", y = "Mean Temperature [*C]")
  

#Stacking multiple plots together
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
# 
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
# 
#   numPlots = length(plots)
# 
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
# 
#   if (numPlots==1) {
#     print(plots[[1]])
# 
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
# 
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
# 
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# multiplot(plot15,plot17,plot20,1)

