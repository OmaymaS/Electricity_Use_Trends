library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(timeDate)
library(rjson)


#data available here:
#http://data.gov.au/dataset/4e21dea3-9b87-4610-94c7-15a8a77907ef/resource/c7901c40-28e2-4601-bd3b-38156428671a/download/SGSC-CTHANPlug-Readings.7z
#download the data, rename it to Book2 and place it in ../data folder

#read data
d1<-read.csv("../data/Book2.csv",stringsAsFactors = F,header = T)

##############################

###define functions to split date
GetTime <-function(x)
{
        unlist(strsplit(x," "))[2]
}

GetDate <-function(x)
{
        unlist(strsplit(x," "))[1]
}

##############################


##cleaning and editing data
dat2<-d1 %>%
        mutate(Date= GetDate(READING_DATETIME),
               Time= map(READING_DATETIME,GetTime),
               # Time=do.call(rbind, Time),
               Date=parse_date_time(Date, orders="d/m/y"),
               WeekDay=1*isWeekday(Date),
               CUSTOMER_ID=factor(CUSTOMER_ID)
        )

dat2$Time<-do.call(rbind, dat2$Time)


#####Function to find average percentage general supply per hour
UserTrend<-function(df)
{
        df %>% 
                group_by(Time) %>%
                summarise(Mean_Time=mean(GENERAL_SUPPLY_KWH,na.rm=T),
                          Median_Time=median(GENERAL_SUPPLY_KWH,na.rm=T))%>%
                mutate(Mean_Hour_percent=100*Mean_Time/sum(Mean_Time),
                       Median_Hour_percent=100*Median_Time/sum(Median_Time),
                       Stamp=as.numeric(gsub(":","",Time))
                       )
        
}

####Filter data
UC<-dat2 %>%
        group_by(CUSTOMER_ID) %>%
        nest() %>%
        mutate(HourAv=map(data,UserTrend)) %>%
        mutate(TSmedian=map(HourAv,function(df) ts(df$Mean_Hour_percent))) %>%
        mutate(TSmean=map(HourAv,function(df) ts(df$Median_Hour_percent)))


##########
#plot all the customer's trend
g1<-ggplot(unnest(UC,HourAv), 
          aes(x=Stamp ,y=Median_Hour_percent, colour=CUSTOMER_ID))+
        geom_line()+
        guides(colour=F)+
        labs(x="Time Stamp (from 00:00 > 24:00)",
             y="Mean Percentage General Supply Per Hour")
plot(g1)       

###Clustering

####Cluster the time series
ts_all<-as.data.frame(do.call(rbind, UC$TSmedian))
hc <- hclust(dist(ts_all), method = "ave")
memb <- cutree(hc, k = 6)
table(memb)

#select users that belong to cluster 1
C1<-UC[memb==1,]

#plot the trend of the first cluster
g2<-ggplot(unnest(C1,HourAv), 
       aes(x=Stamp ,y=Median_Hour_percent))+
        geom_smooth(se=F)+
        guides(colour=F)+
        labs(x="Time Stamp (from 00:00 > 24:00)",
             y="Mean Percentage General Supply Per Hour")

plot(g2)