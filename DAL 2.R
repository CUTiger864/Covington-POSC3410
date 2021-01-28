# Lesson 4 ####


install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
# listing flight data charts ####
data(flights)
filter(flights, month==3, day==14)
(flights, month==3, day==14)<-Mar14
myFirstVar<- "Mar14"
Mar14<-(flights, month==3, day==14)

filter(flights, month == 5 | month == 6)
filter(flights, (arr_delay > 60 & dep_delay < 1))
filter(flights, (arr_delay > 60 & dep_delay < 15))
filter(flights, (arr_delay > 60 & dep_delay > 15))
filter(flights, (dest_GSP)
       filter(flights, air_time > 180)       
       
       NA == 99       
       NA > 3 
       NA < 3       
       Clemson <- NA
       OSU <- NA
       Clemson == OSU
       
       # Clemson and OSu have to same total of victories due to info on their victories being unknown
       
       is.na(Clemson)
       is.na(OSU)
       is.na(flights)
       
       #the data frame for all the flight listings that have information available, none of them are NA
       
       filter(flights, is.na(arr_delay))
       filter(flights, !is.na(arr_delay))
       nrow(filter(flights, is.na(arr_delay)))
       nrow(filter(flights, !is.na(arr_delay)))
       nrow(filter(flights, is.na(dep_delay)))
       nrow(filter(flights, !is.na(dep_delay)))
       flights_df<-filter(flights, (!is.na(arr_delay) & !is.na(dep_delay)))
       flights_df
       flights_df<-filter(flights, (!is.na(arr_delay) & !is.na(dep_delay) & !is.na(year) & !is.na(month) & !is.na(carrier) & !is.na(flight) & !is.na(air_time)))
       flights_df
       
       sample<- sequence(10, from=10L, by=-1L )
       sample_df <- as_tibble(sample)
       sample_df
       
       # a descending order of numbers from 10 to 1 were listed #
       
       arrange(sample_df, value)
       
       # an ascending number order from 1 to 10 was listed #
       
       arrange(df, desc(varible))
       arrange_df<-arrange(flights, arr_delay)
       tail(arrange_df)
       arrange(flights, arr_delay)
       arrange(flights, dep_time)
       
       select
       select(flights, year, month, day, carrier, flight)
       select(flights, year:arr_delay)
       select(flights, -tailnum)
       select(flights, dep_delay:time_hour)
       flights_df<-select(flights, year, month, day, dep_delay, arr_delay, dest, distance)
       flights_df
       
       names(sample_df)
       rename(sample_df, count = value)
       sample_df <- rename(sample_df, count = value)
       names(flights)
       rename(arr_delay, arr_delay='arrival delay')
       flights <- (arr_delay, arr_delay='arrival delay',dep_delay, dep_delay='departure delay) )
rename(dep_delay, dep_delay='departure delay')
flights

flights_small <- select(flights, 
                        year:day,
                        ends_with("delay"),
                        distance,
                        air_time,
                        dest
                        )
mutate(flights_small,
       speed = distance / air_time * 60)                        
mutate(flights_small,
       dest = factor(dest))       
mutate(flights_small,
       'dep_delay' - 'arr_delay'))
mutate(flights_small,
       'arr_time' / 60)
       
       
       
 
 almaMater1<-"Where the Blue Ridge yawns its greatness; Where the Tigers play; Here the sons of dear Old Clemson, Reign supreme alway."      
 almaMater2<-"We will dream of great conquests For our past is grand, And her sons have fought and conquered Every foreign land."
 almaMater3 <- "Where the mountains smile in grandeur O’er the hill and dale; Here the Tiger lair is nestling Swept by storm and gale."      
 almaMater4 <- "We are brothers strong in manhood For we work and strive; and our Alma Mater reigneth Forever in our lives."
 almaMaterChorus <- "Dear Old Clemson, we will triumph And with all our might That the Tiger’s roar may echo O’er the mountain height."
 almaMater<- paste0(almaMater1," ", almaMater2," ", almaMater3," ", almaMater4," ", almaMaterChorus)
  almaMater   
       
 summarise(flights, avgArrDelay= mean(arr_delay, na.rm=TRUE))      
 flights_carrier <- group_by(flights, carrier)
 summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm=TRUE))
 summarise(flights, )
 not_cancelled <- filter(flights, !is.na(dep_delay) & !is.na(arr_delay))
 not_cancelled_year<- group_by(not_cancelled, month)
not_cancelled_year<- group_by(not_cancelled, carrier) 
summarise(flights_carrier, avgDepDelay=mean(dep_delay, na.rm=TRUE))
summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm=TRUE))



# Lesson 5 ####


library(nycflights13)
data("flights")
flights_extract<-filter(flights, month==11 | month == 12 & arr_delay>60)
flights_extract <- group_by(flights_extract,dest )
flights_extract <- count(flights_extract)
flights_extract <- rename(flights_extract, number = n )
flights_extract <- arrange(flights_extract, desc(number))
flights_extract <- head(flights_extract, n=6)
ggplot(flights_extract, aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")
 %>% 
flights_extract<-filter(flights, month== 5 | month == 6)
flights_extract
flights_extract<-filter(flights, month==4)
flights_extract
filter(flights, !is.na(carrier==delta)))




