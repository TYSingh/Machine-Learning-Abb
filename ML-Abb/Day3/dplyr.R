library(dplyr)
library(hflights)
attach(hflights)
?hflights

#Manipulate variables
#1 Average value <- select(),filter(),summarise(),mutate(),arrange()
hflights <- tbl_df(hflights)
hflights
#2
g <- select(hflights,ActualElapsedTime,AirTime,ArrDelay,DepDelay)
g
g[1,]
#3
h <- select(hflights,Origin,Dest,Distance,TaxiIn,TaxiOut,Cancelled)  # we can write as Origin:Cancelled
h
#4
i <- select(hflights,c(Year:DayOfWeek,ArrDelay:Diverted))
i
#Or
select(hflights,c(1:4,12:21))
#Or
select(hflights,-c(DepTime:AirTime))
#6
select(hflights,UniqueCarrier, ends_with("Num"),starts_with("Cancel") )

#7
f = mutate(hflights, GroundTime = TaxiIn + TaxiOut)
f
#8
"a"%in% c("b","a","c")
f1 <- filter(hflights,Distance > 3000)
f1

#9
f3 <- filter(hflights, TaxiIn + TaxiOut > AirTime)
f3

##OR

filter(f, GroundTime > AirTime)

#10

CanWeekend <- filter(hflights,DayOfWeek > 5, Cancelled == 1)
CanWeekend

#Or
filter(hflights, DayOfWeek %in% c(6,7),Cancelled == 1)

#11
arrange(hflights, UniqueCarrier, desc(DepDelay))

#12
arrange(hflights, DepDelay + ArrDelay)

#13
f <- filter(hflights,Dest == "DFW", DepTime < 800)
f
arrange(f,desc(AirTime))
#OR
arrange(filter(hflights, Dest == "DFW",DepTime <800),desc(AirTime))
#OR
hflights %>%
  filter(Dest == "DFW", DepTime < 800) %>% arrange(desc(AirTime))

#14

summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))

##OR

hflights %>%
  summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))
  
#15
hflights %>%
  filter(Diverted == 1) %>%
  summarise(max_div = max(Distance))

#16
hflights %>%
  summarise(n_obs = n(),
            n_carriers = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest,100))

#17
p <- hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(diff != "NA") %>%
  summarise(avg = mean(diff))
p

#18

hflights %>%
  select(Dest,UniqueCarrier,Distance,ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime +100)

#19

hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm =T)) %>%
  arrange(avg_delay,n_canc)
