# Intro to ggplot2

library(ggplot2)
?diamonds
attach(diamonds)

# Visualize the cut variable in the diamonds dataset using a barchart

ggplot(diamonds,aes(x=cut)) + geom_bar()

# Use the BOD
?BOD
attach(BOD)
ggplot(BOD,aes(x=Time,y=demand)) + geom_bar(stat = "identity")  
# if you want the second variable ,write stat = identity

# Notice that there is a gap at 6, this is because the time variable is continuous and not categorical

str(BOD)
ggplot(BOD,aes(x=as.factor(Time),y=demand)) + geom_bar(stat = "identity",col = "light blue",
                               fill = "pink") + xlab("Time") + ylab("Demand")

# Grouping bars together in ggplot2

ggplot(diamonds,aes(x=cut, fill = clarity)) + geom_bar()

ggplot(diamonds,aes(x=cut, fill = clarity)) + geom_bar(position = "dodge",col = "black")
ggplot(diamonds,aes(x=cut, fill = clarity)) + geom_bar(position = position_dodge(1))

ggplot(diamonds,aes(x=cut, fill = clarity)) + geom_bar(position = position_dodge(1))+coord_flip()

#Use our own colours

library(gcookbook)
?uspopchange
head(uspopchange)
attach(uspopchange)
library(dplyr)

# Create a dataframe that has the top ten states in terms of population change

f <- arrange(uspopchange,desc(Change))
f1 <-filter(top_n(f,10))   

#Or
uspopchange %>%
  arrange(desc(Change))

ggplot(f1,aes(x=reorder(Abb,Change),y=Change,fill = Region)) + geom_bar(stat = "identity")  
 
# Manually change the colour of the bars
ggplot(f1,aes(x=reorder(Abb,Change),y=Change,fill = Region)) + geom_bar(stat = "identity") +
       scale_fill_manual(values = c("pink","light blue"))
#Look the site colourbrewer

ggplot(f1,aes(x=reorder(Abb,Change),y=Change,fill = Region)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink","#99d8c9"))

# for line chart geom_line, and many charts can be made
# For horizontal bar chart

ggplot(f1,aes(x=reorder(Abb,Change),y=Change,fill = Region)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink","#99d8c9")) +
 coord_flip()

# whatever we have mentioned in the first line that colour will come
ggplot(f1,aes(x=reorder(Abb,Change),y=Change,fill = Region)) + geom_bar(stat = "identity",fill="red") +
  scale_fill_manual(values = c("pink","#99d8c9")) +
  coord_flip()

#Create a dot plot- helpful for more than 4-5 categories

?tophitters2001
attach(tophitters2001)

data <- tophitters2001 %>%
  select(name,avg) %>%
  slice(1:25)

data
ggplot(data,aes(x=avg,y = name)) +
  geom_point()

ggplot(data,aes(x=avg,y = reorder(name,avg))) +
  geom_point(col = "Turquoise",size = 8)

