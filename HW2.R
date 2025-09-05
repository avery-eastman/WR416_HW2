library(tidyverse)
library(lubridate)
library(ggplot2)

rainwater_data = read_csv('hw2data.csv')

clean_data <- na.omit(rainwater_data)

capacity = 48

clean_data$date <- mdy(clean_data$date)

clean_data$demand <- ifelse(clean_data$P_in>0,0,7)

clean_data$volume <- clean_data$P_in*431*0.93*7.48/12

clean_data$vol_net <- 0

for (i in 2:nrow(clean_data)) {
  clean_data[i,'vol_net']=ifelse((clean_data[i-1,'vol_net']+clean_data[i,'volume'])<=capacity,
                                 clean_data[i-1,'vol_net']+clean_data[i,'volume']-clean_data[i,'demand'],
                                 capacity)
  clean_data[i,'vol_net']=ifelse(clean_data[i,'vol_net']<0,0,clean_data[i,'vol_net'])
}

ggplot()+geom_line(data = clean_data, aes(x=date, y=vol_net))+
  ylab('Volume (gallons)')+
  xlab('Date')

days_empty_48 = sum(clean_data$vol_net == 0)

capacity_2nd = 100

data_100 <- clean_data

data_100$vol_net <- 0

for (i in 2:nrow(data_100)) {
  data_100[i,'vol_net']=ifelse((data_100[i-1,'vol_net']+data_100[i,'volume'])<=capacity_2nd,
                               data_100[i-1,'vol_net']+data_100[i,'volume']-data_100[i,'demand'],
                                 capacity_2nd)
  data_100[i,'vol_net']=ifelse(data_100[i,'vol_net']<0,0,data_100[i,'vol_net'])
}

ggplot()+geom_line(data = data_100, aes(x=date, y=vol_net))+
  ylab('Volume (gallons)')+
  xlab('Date')

days_empty_100 = sum(data_100$vol_net == 0)

