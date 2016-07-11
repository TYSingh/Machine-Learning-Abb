library(lubridate)

#Before Lubridate
date <- "18-03-2016"
date
date + 1

date <- as.POSIXct("18-03-2016", format = "%d-%m-%Y")
date

# After lubridate

date <- dmy("18-03-2016")
date
date <- mdy("03-18-2016")
date
date <- ymd("2016-03-18")
date
date <- dmy("18032016")
date
date <- dmy_hms("18-03-2016 11:30:34")
date
dmy("12-24-2016")
dmy(c("12-01-2015","13-01-2015"))

date <- dmy("29-02-2015")

date <- now()
date
minute(date)
wday(date)
wday(date,label = T,abbr = F)
month(date,label = T)
date
minute(date) <- 4
date

update(date,tz = "UTC")
# lubridate uses UTC

#Arithmatic in dates

start_2015 <- mdy_hms("01-01-2015 00:00:00")
start_2015
start_2015 + days(1)
start_2015 + minutes(12)
start_2015 + years(1)
start_2015 + days(365)
start_2016 <- start_2015 + years(1)
start_2016
start_2016 + days(365)
start_2016 + years(1)


#collision rda

library(dplyr)
library(lubridate)

a = 10
b = 2
saveRDS(a,"myfile.rda")
load("myfile.rda")

load("collision.rda")
attach(collision)
data <- collision
data

glimpse(data)

# date and time of the collision are factors. In order to use them as date objects,then we need to use
#lubridate

# create a timestamp
time_stamp <-
  paste(data$Collision.date, data$Collision.Time,sep = " ")
head(time_stamp)

# transform the timestamp into date object
time_stamp = dmy_hms(time_stamp)

# add the time stamp to the data frame
data$time_stamp = time_stamp
View(data)

# Filter the data to explore only collisions with padestrains

ped <- filter(data,Involved.With == "Pedestrian")
View(ped)

# Create the bar chart to check the no. of accidents on each day of the week

weekday <- wday(ped$time_stamp, label = T, abbr = T)
ped$weekday <- weekday

ggplot(data = ped,aes(x = weekday,y = Accident.Number)) +
  geom_bar(stat = "identity",col = "Turquoise")

# For Month , Control+Shift+A - Indendation
month <- month(ped$time_stamp, label = T, abbr = T)
ped$month <- month

ggplot(data = ped,aes(x = month,y = Accident.Number)) +
  geom_bar(stat = "identity",col = "Turquoise")

# Create a heat map that shows the accidents on each hour of each day

heatmapData <- ped %>%
  mutate( Time = hour(time_stamp)) %>%
  group_by(weekday,Time) %>%
  summarise(count=n())

#Visualize
ggplot(heatmapData,aes(x = weekday, y= as.factor(Time),fill =count)) +
  geom_tile() +
  scale_fill_gradient(low = "white",high = "dark red") +
  ylab ("Hour")+
  xlab("")